{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- TODO switch to TVars?

module Web.Scotty.Login.Session ( initializeCookieDb
                                , addSession
                                , authCheck
                                , SessionConfig(..)
                                , defaultSessionConfig
                                , SessionStore
                                )
       where
import           Control.Concurrent                (forkIO, threadDelay)
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text                         as TS
import qualified Data.Text.Lazy                    as T
import           Data.Time.Clock
import           Database.Persist                  as D
import           Database.Persist.Sqlite
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


data SessionConfig = SessionConfig { dbName             :: String
                                   , syncInterval       :: Int --seconds
                                   , expirationInterval :: NominalDiffTime
                                   }
{- data Session
  = Session {sessionSid :: !T.Text, sessionExpiration :: !UTCTime} -}

 -- is this the best way to do this?
type SessionVault = [Session]

type SessionStore = IORef SessionVault


defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig "sessions.sqlite3" 60 120

{-# NOINLINE vault #-}
vault :: SessionStore
vault = unsafePerformIO $ newIORef []

readVault :: IO SessionVault
readVault = readIORef vault

modifyVault :: (SessionVault -> SessionVault) -> IO ()
modifyVault f = atomicModifyIORef' vault (flip (,) () . f)

initializeCookieDb :: SessionConfig -> IO ()
initializeCookieDb c =  do
  t <- getCurrentTime
  ses <- runDB c $ do runMigration migrateAll
                      selectList [SessionExpiration >=. t] []
  let sessions = (map entityVal ses) :: SessionVault
  modifyVault $ const sessions
  forkIO $ dbSyncAndCleanupLoop c
  return ()

dbSyncAndCleanupLoop :: SessionConfig  -> IO ()
dbSyncAndCleanupLoop c = do
  threadDelay $ syncInterval c * 1000000
  t <- getCurrentTime
  vaultContents <- readVault
  mapM_ (runDB c . insert) vaultContents
  runDB c $ deleteWhere [SessionExpiration <=. t]
  modifyVault $ filter (\s -> sessionExpiration s >= t)
  dbSyncAndCleanupLoop c -- tail (hopefully) recurse

addSession :: SessionConfig  -> ActionT T.Text IO () -- (Key Session)
addSession c = do
  (bh :: B.ByteString) <- liftIO $ getRandomBytes 128
  t <- liftIO getCurrentTime
  let val = TS.pack $ mconcat $ map (`showHex` "") $ B.unpack bh
      t' = addUTCTime (expirationInterval c) t -- ten minutes
  C.setSimpleCookieExpr "SessionId" val t'
  liftIO $ insertSession (T.fromStrict val) t'

-- denial action, approval action
authCheck :: (MonadIO m, ScottyError e)
             => ActionT e m ()
             -> ActionT e m ()
             -> ActionT e m ()
authCheck d a = do
  vaultContents <- liftIO $ readVault
  c <- SC.getCookie "SessionId"
  case c of
   Nothing -> d
   Just v -> do -- Text
     -- liftIO $ runDB conf $ selectFirst [SessionSid ==. T.fromStrict v] []
     let session = find (\s -> sessionSid s == T.fromStrict v) vaultContents
     case session of
      Nothing -> d
      Just s -> do let -- s = entityVal e
                     t = sessionExpiration s
                   curTime <- liftIO getCurrentTime
                   if diffUTCTime t curTime > 0
                     then a
                     else d -- this shouldnt happen, browser should delete it


-- now have to sync in-mem db to sqlite db
insertSession :: T.Text
                 -> UTCTime
                 -> IO ()
insertSession sid t = modifyVault $ \v' -> (Session sid t) : v'

runDB :: SessionConfig
         -> SqlPersistT (NoLoggingT (ResourceT IO)) a
         -> IO a
runDB c = runSqlite $ TS.pack $ dbName c
