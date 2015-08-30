{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Scotty.Login.Session ( initializeCookieDb
                                , addSession
                                , authCheck
                                )
       where

import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.Text                         as TS
import qualified Data.Text.Lazy                    as T
import           Data.Time.Clock
import           Database.Persist                  as D
import           Database.Persist.Sqlite
import           System.Random                     (randomIO)
import           Web.Scotty.Cookie                 as SC
import           Web.Scotty.Trans                  as S

import           Web.Scotty.Login.Internal.Cookies as C
import           Web.Scotty.Login.Internal.Model

initializeCookieDb :: IO ()
initializeCookieDb = void $ do
  runDB $ runMigration migrateAll
  forkIO dbEraseLoop -- erase expired sessions


dbEraseLoop :: IO ()
dbEraseLoop = do threadDelay 60000000 -- 1 minute
                 print "deleting expired sessionids..."
                 t <- getCurrentTime
                 runDB $ deleteWhere [SessionExpiration <=. t]
                 dbEraseLoop -- recurse


addSession ::  ActionT T.Text IO (Key Session)
addSession = do
  h <- liftIO (randomIO :: IO Int)
  t <- liftIO getCurrentTime
  let t' = addUTCTime 600 t -- ten minutes
      val = TS.pack $ show h
  C.setSimpleCookieExpr "SessionId" val t'
  liftIO $ insertSession (T.fromStrict val) t'

-- denial action, approval action
authCheck :: (MonadIO m, ScottyError e)
             => ActionT e m ()
             -> ActionT e m ()
             -> ActionT e m ()
authCheck d a = do
  c <- SC.getCookie "SessionId"
  case c of
   Nothing -> d
   Just v -> do -- Text
     session <- liftIO $ runDB $ selectFirst [SessionSid ==. T.fromStrict v] []
     case session of
      Nothing -> d
      Just e -> do let s = entityVal e
                       t = sessionExpiration s
                   curTime <- liftIO getCurrentTime
                   if diffUTCTime t curTime > 0
                     then a
                     else d -- this shouldnt happen, browser should delete it



insertSession :: T.Text -> UTCTime -> IO (Key Session)
insertSession sid t = runDB $ insert $ Session sid t

runDB = runSqlite "db.sqlite3"

void :: (Monad m) => m a -> m ()
void a = a >> return ()
