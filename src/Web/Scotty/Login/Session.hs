{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}


-- TODO switch to TVars?

{-|
Module:              Web.Scotty.Login.Session
Description:         Simple library for Scotty sessions and authorization
Copyright:           (c) Miles Frankel, 2017
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
  S.get \"\/logout\" $ removeSession conf
@
-}

module Web.Scotty.Login.Session ( initializeCookieDb
                                , addSession
                                , removeSession
                                , authCheck
                                , authCheckWithSession
                                , SessionConfig(..)
                                , Session(..)
                                , defaultSessionConfig
                                )
       where
import           Control.Concurrent                (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Maybe
import           Crypto.Random                     (getRandomBytes)
import qualified Data.ByteString                   as B
import qualified Data.Text                         as TS
import qualified Data.Text.Lazy                    as T
import           Data.Time.Clock
import           Database.Persist                  as D
import           Database.Persist.Sqlite
import           Network.HTTP.Types.Status         (forbidden403)
import           Numeric                           (showHex)
import           Web.Scotty.Cookie                 as SC
import           Web.Scotty.Trans                  as S

import           Web.Scotty.Login.Internal.Cookies as C
import           Web.Scotty.Login.Internal.Model

import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource      (ResourceT, runResourceT)
import           Control.Monad.IO.Unlift           (MonadUnliftIO)


import qualified Data.HashMap.Strict               as H
import           Data.IORef
import           System.IO.Unsafe                  (unsafePerformIO)

-- import qualified Data.Map                          as M

-- | Configuration for the session database.
data SessionConfig =
  SessionConfig { dbPath             :: String          -- ^ Path to SQLite database file
                , syncInterval       :: NominalDiffTime -- ^ Time between syncs to database (seconds)
                , expirationInterval :: NominalDiffTime -- ^ Cookie expiration time (seconds)
                , debugMode          :: Bool            -- ^ Debug Mode (extra logging)
                }
{- data Session
  = Session {sessionSid :: !T.Text, sessionExpiration :: !UTCTime} -}

type SessionVault = H.HashMap T.Text Session

type SessionStore = IORef SessionVault

-- | Default settings for the session store. May not be suitable for all applications.
--
-- They are:
--
-- * dbPath = \"sessions.sqlite\",
--
-- * syncInterval = 1200 seconds (20 minutes),
--
-- * expirationInterval = 86400 seconds (1 day)
--
-- * debugMode = False

defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig "sessions.sqlite3" 1200 120 False

{-# NOINLINE vault #-}
vault :: SessionStore
vault = unsafePerformIO $ newIORef H.empty

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
  let sessions = entityVal <$> ses :: [Session]
      seshMap  = H.fromList $ (\s -> (sessionSid s, s)) <$> sessions
  modifyVault $ const seshMap
  forkIO $ dbSyncAndCleanupLoop c
  return ()

dbSyncAndCleanupLoop :: SessionConfig  -> IO ()
dbSyncAndCleanupLoop c = do
  threadDelay $ floor (syncInterval c) * 1000000
  t <- getCurrentTime
  vaultContents <- readVault
  runDB c $ deleteWhere ([] :: [Filter Session]) -- delete all sessions in db
  mapM_ (runDB c . insert) vaultContents -- add vault to db
  modifyVault $ H.filter (\s -> sessionExpiration s >= t)
  dbSyncAndCleanupLoop c -- tail (hopefully) recurse

-- | Add a session. This gives the user a SessionId cookie, and inserts a corresponding entry into the session store. It also returns the Session that was just inserted.
addSession :: SessionConfig -> ActionT T.Text IO Session
addSession c = do
  (bh :: B.ByteString) <- liftIO $ getRandomBytes 128
  t <- liftIO getCurrentTime
  let val = TS.pack $ mconcat $ map (`showHex` "") $ B.unpack bh
      t' = addUTCTime (expirationInterval c) t
      session = Session (T.fromStrict val) t'
  when (debugMode c) $ liftIO $ putStrLn $ "adding session " ++ show session
  C.setSimpleCookieExpr "SessionId" val t'
  liftIO $ insertSession (T.fromStrict val) t'
  return $ Session (T.fromStrict val) t'


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
authCheck d a = authCheckWithSession d (const a)

-- | Check whether a user is authorized, and return the Session that they are authorized for
--
-- Example usage:
--
-- @
--    S.get \"\/auth_test\" $ authCheck (redirect \"\/denied\") $
--      \s -> S.text $ "authorized as " ++ show s
-- @

authCheckWithSession
 :: (MonadIO m, ScottyError e)
             => ActionT e m ()              -- ^ The action to perform if user is denied
             -> (Session -> ActionT e m ()) -- ^ The action to perform if user is authorized
             -> ActionT e m ()
authCheckWithSession d a =
  maybe (d >> status forbidden403) return <=< runMaybeT $ do
    c <- MaybeT $ SC.getCookie "SessionId"
    session <- MaybeT $ H.lookup (T.fromStrict c) <$> liftIO readVault
    curTime <- liftIO getCurrentTime
    guard $ diffUTCTime (sessionExpiration session) curTime > 0 -- this shouldn't abort, browser should delete it
    lift $ a session


-- | Remove a session. Does not take a sessionConfig (doesn't need one)
--
-- Example usage:
--
-- @
--   S.get \"\/logout\" $ removeSession conf
-- @

removeSession :: (ScottyError e, MonadIO m) => SessionConfig -> ActionT e m ()
removeSession c = (SC.getCookie "SessionId" >>=) $ mapM_ $ \sid -> do
  liftIO $ modifyVault $ H.delete (T.fromStrict sid)
  when (debugMode c) $ liftIO $ putStrLn $ "removed session " ++ show sid


-- inserts session into vault
insertSession :: T.Text
                 -> UTCTime
                 -> IO ()
insertSession sid t = modifyVault $ H.insert sid (Session sid t)


runDB
    :: (MonadIO m, MonadUnliftIO m)
    => SessionConfig -> SqlPersistT (LoggingT (ResourceT m)) a -> m a
runDB c = runSqlite' c $ TS.pack $ dbPath c

runSqlite'
    :: (MonadIO m, MonadUnliftIO m) 
    => SessionConfig -> TS.Text -> SqlPersistT (LoggingT (ResourceT m)) a -> m a
runSqlite' conf connstr = runResourceT
                               . runStderrLoggingT
                               . filterLogger (const . const $ debugMode conf)
                               . withSqliteConn connstr
                               . runSqlConn
