{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.Scotty.Login.Session ( initializeCookieDb
                                , addSession
                                , authCheck
                                , SessionConfig(..)
                                , defaultSessionConfig
                                , SessionVault
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

import           Control.Monad.Logger              (NoLoggingT,
                                                    runStderrLoggingT)
import           Control.Monad.Trans.Resource      (ResourceT, runResourceT)

import           Data.IORef
import           Data.List                         (find, nub)
data SessionConfig = SessionConfig { dbName             :: String
                                   , cleanupInterval    :: Int --seconds
                                   , expirationInterval :: NominalDiffTime
                                   }
{- data Session
  = Session {sessionSid :: !T.Text, sessionExpiration :: !UTCTime} -}
 -- inefficient?
type SessionVault = [Session]


defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig "sessions.sqlite3" 60 120

initializeCookieDb :: SessionConfig -> IO (IORef SessionVault)
initializeCookieDb c =  do
  t <- getCurrentTime
  ses <- runDB c $ do runMigration migrateAll
                      selectList [SessionExpiration >=. t] []
  let sessions = (map entityVal ses) :: SessionVault
  vault <- newIORef sessions
  forkIO $ dbSyncAndCleanupLoop c vault
  return vault

dbSyncAndCleanupLoop :: SessionConfig -> IORef SessionVault -> IO ()
dbSyncAndCleanupLoop c v = do
  threadDelay $ cleanupInterval c * 1000000
  putStrLn "Deleting expired sessions from database..."
  t <- getCurrentTime
  dbContentsPre <- runDB c $ selectList  [SessionExpiration >=. t] []

  print "syncloop"
  print "dbPre:" >> print dbContentsPre

  vaultContentsPre <- readIORef v
  print "vaultPre:" >> print vaultContentsPre

  mapM_ (runDB c . insert) vaultContentsPre

  dbContentsPost <- runDB c $ do
    deleteWhere [SessionExpiration <=. t]
    selectList  [SessionExpiration >=. t] []
  print "dbPost:" >> print dbContentsPost

  dbSyncAndCleanupLoop c v -- recurse

addSession :: SessionConfig -> IORef SessionVault -> ActionT T.Text IO () -- (Key Session)
addSession c v = do
  (bh :: B.ByteString) <- liftIO $ getRandomBytes 128
  t <- liftIO getCurrentTime
  let val = TS.pack $ mconcat $ map (`showHex` "") $ B.unpack bh
      t' = addUTCTime (expirationInterval c) t -- ten minutes
  C.setSimpleCookieExpr "SessionId" val t'
  liftIO $ insertSession c v (T.fromStrict val) t'

-- denial action, approval action
authCheck :: (MonadIO m, ScottyError e)
             => SessionConfig
             -> IORef SessionVault
             -> ActionT e m ()
             -> ActionT e m ()
             -> ActionT e m ()
authCheck conf vault d a = do
  vaultContents <- liftIO $ readIORef vault
  liftIO $ print "authCheck vault contents:"
  liftIO $ print $ vaultContents
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


insertSessionDb :: SessionConfig -> T.Text -> UTCTime -> IO (Key Session)
insertSessionDb c sid t = runDB c $ insert $ Session sid t


-- now have to sync in-mem db to sqlite db
insertSession :: SessionConfig
                 -> IORef SessionVault
                 -> T.Text
                 -> UTCTime
                 -> IO ()
insertSession c v sid t = do
  print "insert session vault contents:"
  print =<< readIORef v
  atomicModifyIORef' v $ \v -> (((Session sid t) : v), ())

runDB :: SessionConfig
         -> SqlPersistT (NoLoggingT (ResourceT IO)) a
         -> IO a
runDB c = runSqlite $ TS.pack $ dbName c

void :: (Monad m) => m a -> m ()
void a = a >> return ()
