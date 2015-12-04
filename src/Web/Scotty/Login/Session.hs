{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- TODO switch to TVars?

{-|
Module:              Web.Scotty.Login.Session
Description:         Simple library for Scotty sessions and authorization
Copyright:           (c) Miles Frankel, 2015
License:             GPL-2

A Simple library for session adding and checking, with automatic SQLite backup of session store. The session store is kept in memory for fast access. Session cookie expiration and database syncing timing are configurable. Note that this packages does not handle user authorization; you will have to roll your own (the package persistent is recommended) or use another package.

Example usage:

@
\{\-\# LANGUAGE OverloadedStrings   \#\-\}
\{\-\# LANGUAGE ScopedTypeVariables \#\-\}

module Main where
import qualified Data.Text.Lazy           as T
import           Web.Scotty               as S
import           Web.Scotty.Login.Session

conf :: SessionConfig
conf = defaultSessionConfig

main :: IO ()
main = do
  initializeCookieDb conf
  scotty 8000 routes

routes :: ScottyM ()
routes = do
  S.get \"/denied\" $ S.text \"access denied\"
  S.get \"/login\" $ do S.html $ T.pack $ unlines $
                        [ \"\<form method=\\\"POST\\\" action=\\\"/login\\\"\>\"
                        , \"\<input type=\\\"text\\\" name=\\\"username\\\"\>\"
                        , \"\<input type=\\\"password\\\" name=\\\"password\\\"\>\"
                        , \"\<input type=\\\"submit\\\" name=\\\"login\\\" value=\\\"login\\\"\>\"
                        , \"\</form\>\" ]
  S.post \"/login\" $ do
    (usn :: String) <- param \"username\"
    (pass :: String) <- param \"password\"
    if usn == \"guest\" && pass == \"password\"
      then do addSession conf
              redirect \"/authed\"
      else do redirect \"/denied\"
  S.get \"\/authcheck\" $ authCheck (redirect \"\/denied\") $
    S.text \"authorized\"
@
-}

module Web.Scotty.Login.Session ( initializeCookieDb
                                , addSession
                                , authCheck
                                , SessionConfig(..)
                                , Session(..)
                                , defaultSessionConfig
                                )
       where
import           Control.Concurrent                (forkIO, threadDelay)
import           Control.Monad                     (when)
import           Control.Monad.IO.Class
import           Data.Maybe                        (isNothing)
import           Data.Monoid
import qualified Data.Text                         as TS
import qualified Data.Text.Lazy                    as T
import           Data.Time.Clock
import           Database.Persist                  as D
import           Database.Persist.Sqlite
import           Network.HTTP.Types.Status         (forbidden403)
import           Web.Scotty.Cookie                 as SC
import           Web.Scotty.Trans                  as S

import           Crypto.Random                     (getRandomBytes)
import qualified Data.ByteString                   as B
import           Numeric                           (showHex)

import           Web.Scotty.Login.Internal.Cookies as C
import           Web.Scotty.Login.Internal.Model

import           Control.Monad.Logger              (NoLoggingT)
--                                                    runStderrLoggingT)
import           Control.Monad.Trans.Resource      (ResourceT)

import           Data.IORef
import           Data.List                         (find)
import           System.IO.Unsafe                  (unsafePerformIO)
-- import qualified Data.Map                          as M

-- | Configuration for the session database.
data SessionConfig =
  SessionConfig { dbPath             :: String          -- ^ Path to SQLite database file
                , syncInterval       :: NominalDiffTime -- ^ Time between syncs to database (seconds)
                , expirationInterval :: NominalDiffTime -- ^ Cookie expiration time (seconds)
                }
{- data Session
  = Session {sessionSid :: !T.Text, sessionExpiration :: !UTCTime} -}

type SessionVault = [Session]

type SessionStore = IORef SessionVault

-- | Default settings for the session store. May not be suitable for all applications.
--
-- They are:
--
-- * dbPath = \"sessions.sqlite\",
--
-- * syncInterval = 1200 seconds (30 minutes),
--
-- * expirationInterval = 86400 seconds (1 day)

defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig "sessions.sqlite3" 1200 120

{-# NOINLINE vault #-}
vault :: SessionStore
vault = unsafePerformIO $ newIORef []

readVault :: IO SessionVault
readVault = readIORef vault

modifyVault :: (SessionVault -> SessionVault) -> IO ()
modifyVault f = atomicModifyIORef' vault (flip (,) () . f)

-- | Reload the session database into memory, and fork the database sync and cleanup thread. This must be called before invoking scotty.
initializeCookieDb :: SessionConfig -> IO ()
initializeCookieDb c =  do
  t <- getCurrentTime
  ses <- runDB c $ do runMigration migrateAll
                      selectList [SessionExpiration >=. t] []
  let sessions = map entityVal ses :: SessionVault
  modifyVault $ const sessions
  forkIO $ dbSyncAndCleanupLoop c
  return ()

dbSyncAndCleanupLoop :: SessionConfig  -> IO ()
dbSyncAndCleanupLoop c = do
  threadDelay $ (floor $ syncInterval c) * 1000000
  t <- getCurrentTime
  vaultContents <- readVault
  runDB c $ deleteWhere [SessionExpiration >=. t] -- delete all sessions in db
  runDB c $ deleteWhere [SessionExpiration <=. t]
  mapM_ (runDB c . insert) vaultContents -- add vault to db
  modifyVault $ filter (\s -> sessionExpiration s >= t)
  dbSyncAndCleanupLoop c -- tail (hopefully) recurse

-- | Add a session. This gives the user a SessionId cookie, and inserts a corresponding entry into the session store. It also returns the Session that was just inserted.
addSession :: SessionConfig -> ActionT T.Text IO (Maybe Session)
addSession c = do
  vc <- liftIO readVault
  liftIO $ print $ "adding session" ++ show vc
  (bh :: B.ByteString) <- liftIO $ getRandomBytes 128
  t <- liftIO getCurrentTime
  let val = TS.pack $ mconcat $ map (`showHex` "") $ B.unpack bh
      t' = addUTCTime (expirationInterval c) t
  C.setSimpleCookieExpr "SessionId" val t'
  liftIO $ insertSession (T.fromStrict val) t'
  return $ Just $ Session (T.fromStrict val) t'


-- | Check whether a user is authorized.
--
-- Example usage:
--
-- @
--    S.get \"\/auth_test\" $ authCheck (redirect \"\/denied\") $
--      S.text "authorized"
-- @
authCheck :: (MonadIO m, ScottyError e)
             => ActionT e m () -- ^ The action to perform if user is denied
             -> ActionT e m () -- ^ The action to perform if user is authorized
             -> ActionT e m ()
authCheck d a = do
  vaultContents <- liftIO readVault
  liftIO $ print $ "checking vault contents: " ++ show vaultContents
  c <- SC.getCookie "SessionId"
  case c of
   Nothing -> d
   Just v -> do -- Text
     -- liftIO $ runDB conf $ selectFirst [SessionSid ==. T.fromStrict v] []
     let session = find (\s -> sessionSid s == T.fromStrict v) vaultContents
     case session of
      Nothing ->  d >> status forbidden403
      Just s -> do let -- s = entityVal e
                     t = sessionExpiration s
                   curTime <- liftIO getCurrentTime
                   if diffUTCTime t curTime > 0
                     then a
                          -- this shouldnt happen, browser should delete it
                     else d >> status forbidden403


-- | Check whether a user is authorized, and return the Session that they are authorized for
--
-- Example usage:
--
-- @
--    S.get \"\/auth_test\" $ authCheck (redirect \"\/denied\") $
--      \s -> S.text $ "authorized as " ++ show s
-- @
authCheckWithSession :: (MonadIO m, ScottyError e)
                        => ActionT e m () -- ^ The action to perform if user is denied
                        -> (Session -> ActionT e m ()) -- ^ The action to perform if user is authorized
                        -> ActionT e m ()
authCheckWithSession d a = do
  vaultContents <- liftIO readVault
  c <- SC.getCookie "SessionId"
  case c of
   Nothing -> d
   Just v -> do -- Text
     -- liftIO $ runDB conf $ selectFirst [SessionSid ==. T.fromStrict v] []
     let session = find (\s -> sessionSid s == T.fromStrict v) vaultContents
     case session of
      Nothing ->  d >> status forbidden403
      Just s -> do let -- s = entityVal e
                     t = sessionExpiration s
                   curTime <- liftIO getCurrentTime
                   if diffUTCTime t curTime > 0
                     then return s >>= a
                          -- this shouldnt happen, browser should delete it
                     else d >> status forbidden403


-- now have to sync in-mem db to sqlite db
insertSession :: T.Text
                 -> UTCTime
                 -> IO ()
insertSession sid t = modifyVault (Session sid t :)

runDB :: SessionConfig
         -> SqlPersistT (NoLoggingT (ResourceT IO)) a
         -> IO a
runDB c = runSqlite $ TS.pack $ dbPath c
