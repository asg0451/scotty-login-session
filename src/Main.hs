{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Network.HTTP.Types.Status (unauthorized401)
import           Web.Scotty                as S
-- import           Web.Scotty.Cookie         as C

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy  as L
import           Control.Monad.Trans
import           Data.Functor
import           Data.Monoid
import qualified Data.Text                 as TS
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as T
import           Data.Time.Clock
import           System.Environment        (getEnv)

import           Database.Persist          as D
import           Database.Persist.Sqlite
import           Model
import           System.Random             (randomIO)

import           ScottyCookie              as C

import           Control.Concurrent

main :: IO ()
main = do
  p <- getEnv "PORT"
  liftIO $ runDB $ runMigration migrateAll
  forkIO dbEraseLoop -- erase expired sessions
  scotty (read p) $ do
    routes

dbEraseLoop :: IO ()
dbEraseLoop = do threadDelay 60000000 -- 1 minute
                 print "deleting expired sessionids..."
                 t <- getCurrentTime
                 runDB $ deleteWhere [SessionExpiration <=. t]
                 dbEraseLoop -- recurse


routes :: ScottyM ()
routes = do
  S.get "/" $ S.text "home"
  S.get "/denied" $ S.text "login denied -- wrong username or password"
  S.get "/login" $ do c <- liftIO $ readFile "static/login.html"
                      S.html $ T.pack $ c
  S.post "/login" $ do
    (usn :: String) <- param "username"
    (pass :: String) <- param "password"
    doOrDeny (usn == "miles" && pass == "password") $ do
      t <- liftIO $ getCurrentTime
      let t' = addUTCTime 600 t -- ten minutes
      h <- liftIO $ (randomIO :: IO Int)
      let val = TS.pack $ show h
      setSimpleCookieExpr "SessionId" val t'
      liftIO $ insertSession (T.fromStrict val) t'
      redirect "/authed"
  S.get "/authed" $ authCheck $ do
    S.text "authed"

authCheck :: ActionM () -> ActionM ()
authCheck a = do
  c <- getCookie "SessionId"
  case c of
   Nothing -> S.text "auth cookie not found"
   Just v -> do -- Text
     session <- liftIO $ runDB $ selectFirst [SessionSid ==. T.fromStrict v] []
     case session of
      Nothing -> S.text "auth cookie found but not in db"
      Just s -> do let v = entityVal s
                       t = sessionExpiration v
                   curTime <- liftIO $ getCurrentTime
                   if (diffUTCTime t curTime > 0)
                     then a
                     else  -- this shouldnt happen, browser should delete it
                         S.text "cookie expired, please login again"


insertSession :: T.Text -> UTCTime -> IO (Key Session)
insertSession sid t = runDB $ insert $ Session sid t

runDB = runSqlite "db.sqlite3"

doOrDeny :: Bool -> ActionM () -> ActionM ()
doOrDeny p s = if p then s else do S.status unauthorized401
                                   redirect "/denied"
