{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.Scotty.Login.Session ( initializeCookieDb
                                , addSession
                                , authCheck
                                , SessionConfig(..)
                                , defaultSessionConfig
                                )
       where
import           Control.Concurrent                (forkIO, threadDelay)
import           Control.Monad.IO.Class
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
import           Control.Monad.Trans.Resource      (ResourceT)

data SessionConfig = SessionConfig { dbName             :: String
                                   , cleanupInterval    :: Int --seconds
                                   , expirationInterval :: NominalDiffTime
                                   }



defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig "sessions.sqlite3" 60 120

initializeCookieDb :: SessionConfig -> IO ()
initializeCookieDb c = void $ do
  runDB c $ runMigration migrateAll
  forkIO $ dbEraseLoop c -- erase expired sessions

dbEraseLoop :: SessionConfig -> IO ()
dbEraseLoop c = do threadDelay $ cleanupInterval c * 1000000
                   putStrLn "Deleting expired sessions from database..."
                   t <- getCurrentTime
                   runDB c $ deleteWhere [SessionExpiration <=. t]
                   dbEraseLoop c -- recurse

addSession :: SessionConfig -> ActionT T.Text IO (Key Session)
addSession c = do
  (bh :: B.ByteString) <- liftIO $ getRandomBytes 128
  let bs = map (`showHex` "") $ B.unpack bh
      h = mconcat bs
  t <- liftIO getCurrentTime
  let t' = addUTCTime (expirationInterval c) t -- ten minutes
      val = TS.pack h
  C.setSimpleCookieExpr "SessionId" val t'
  liftIO $ insertSession c (T.fromStrict val) t'

-- denial action, approval action
authCheck :: (MonadIO m, ScottyError e)
             => SessionConfig
             -> ActionT e m ()
             -> ActionT e m ()
             -> ActionT e m ()
authCheck conf d a = do
  c <- SC.getCookie "SessionId"
  case c of
   Nothing -> d
   Just v -> do -- Text
     session <- liftIO $ runDB conf $ selectFirst [SessionSid ==. T.fromStrict v] []
     case session of
      Nothing -> d
      Just e -> do let s = entityVal e
                       t = sessionExpiration s
                   curTime <- liftIO getCurrentTime
                   if diffUTCTime t curTime > 0
                     then a
                     else d -- this shouldnt happen, browser should delete it


insertSession :: SessionConfig -> T.Text -> UTCTime -> IO (Key Session)
insertSession c sid t = runDB c $ insert $ Session sid t

runDB :: SessionConfig
         -> SqlPersistT (NoLoggingT (ResourceT IO)) a
         -> IO a
runDB c = runSqlite $ TS.pack $ dbName c

void :: (Monad m) => m a -> m ()
void a = a >> return ()
