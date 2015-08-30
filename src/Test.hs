{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text.Lazy           as T
import           Web.Scotty               as S
import           Web.Scotty.Login.Session

import           Control.Monad.IO.Class   (liftIO)

main :: IO ()
main = do
  initializeCookieDb
  scotty (4040) routes

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
      else do redirect "/denied"
  S.get "/authed" $ authCheck (redirect "/denied") $
    S.text "authed"
