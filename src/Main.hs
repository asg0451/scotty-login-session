{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import           Web.Scotty.Trans         as S

import           Network.HTTP.Types.Status (unauthorized401)
import           Web.Scotty                as S
import           Web.Scotty.Cookie         as C

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy  as L
import           Control.Monad.Trans
import           Data.Functor
import           Data.Monoid
import qualified Data.Text                 as TS
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as T


import           Database.Persist          as D
import           Database.Persist.Sqlite
import           Model
import           System.Random             (randomIO)

main :: IO ()
main = do
  liftIO $ runDB $ runMigration migrateAll
  scotty 4242 $ do
    routes


routes = do
  S.get "/" $ S.text "home"
  S.get "/denied" $ S.text "login denied -- wrong username or password"
  S.get "/login" $ do c <- liftIO $ readFile "static/login.html"
                      S.html $ T.pack $ c
  S.post "/login" $ do
    (usn :: String) <- param "username"
    (pass :: String) <- param "password"
    doOrDeny (usn == "miles" && pass == "password") $ do
      h <- liftIO $ (randomIO :: IO Int)
      let val = TS.pack $ show h
      setSimpleCookie "SessionId" val
      liftIO $ insertSession $ T.fromStrict val
      redirect "/"
  S.get "/authed" $ do
    c <- getCookie "SessionId"
    case c of
     Nothing -> S.text "auth cookie not found"
     Just v -> do -- Text
       session <- liftIO $ runDB $ selectFirst [SessionSid ==. T.fromStrict v] []
       case session of
        Nothing -> S.text "auth cookie found but not in db"
        Just s -> do
          S.text "authed"



insertSession sid = runDB $ insert $ Session sid

runDB = runSqlite "db.sqlite3"

doOrDeny p s = if p then s else do S.status unauthorized401
                                   redirect "/denied"

-- type Env = [(String, String)]
-- type StateIO = StateT Env IO -- still takes ret val

-- type ActionM' = ActionT T.Text StateIO

-- s = [("a", "30")] :: Env

-- main = do
--   scottyT 4242 (`evalStateT` s) $ do
--     S.get "/" $ app'

-- app' :: ActionM' ()
-- app' =  do
--   v <- lift $ L.get
--   html $ T.pack $ show $ v
