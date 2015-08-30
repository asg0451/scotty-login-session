{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text.Lazy            as T
import           Session
import           Web.Scotty                as S

import           Control.Monad.IO.Class    (liftIO)
import           Network.HTTP.Types.Status (unauthorized401)
import           System.Environment        (getEnv)

main :: IO ()
main = do
  initializeCookieDb
  p <- getEnv "PORT"
  scotty (read p) routes

routes :: ScottyM ()
routes = do
  S.get "/" $ S.text "home"
  S.get "/denied" $ S.text "login denied -- wrong username or password"
  S.get "/login" $ do c <- liftIO $ readFile "static/login.html"
                      S.html $ T.pack c
  S.post "/login" $ do
    (usn :: String) <- param "username"
    (pass :: String) <- param "password"
    if usn == "guest" && pass == "password"
      then do addSession
              redirect "/authed"
      else do S.status unauthorized401
              redirect "/denied"
  S.get "/authed" $ authCheck (redirect "/denied") $
    S.text "authed"
