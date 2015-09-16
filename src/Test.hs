{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.IORef
import qualified Data.Text.Lazy           as T
import           Web.Scotty               as S
import           Web.Scotty.Login.Session

conf :: SessionConfig
conf = defaultSessionConfig

main :: IO ()
main = do
  sv <- initializeCookieDb conf
  scotty 4040 (routes sv)

routes :: SessionStore -> ScottyM ()
routes sv = do
  S.get "/" $ S.text "home"
  S.get "/denied" $ S.text "login denied -- wrong username or password"
  S.get "/login" $ do S.html $ T.pack $ unlines $
                        ["<form method=\"POST\" action=\"/login\">"
                        , "<input type=\"text\" name=\"username\">"
                        , "<input type=\"password\" name=\"password\">"
                        , "<input type=\"submit\" name=\"login\" value=\"login\">"
                        , "</form>"]
  S.post "/login" $ do
    (usn :: String) <- param "username"
    (pass :: String) <- param "password"
    if usn == "guest" && pass == "password"
      then do addSession conf sv
              redirect "/authed"
      else do redirect "/denied"
  S.get "/authed" $ authCheck sv (redirect "/denied") $
    S.text "authed"
