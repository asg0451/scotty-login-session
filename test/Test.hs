{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where
import           Control.Concurrent       (forkIO, killThread)
import           Network.Wai.Test
import           System.Exit              (exitFailure)
import           Web.Scotty
import           Web.Scotty.Login.Session
main = do
  tid <- forkIO runScotty




conf :: SessionConfig
conf = defaultSessionConfig

runScotty :: IO ()
runScotty = do
  initializeCookieDb
  scotty 4040 routes

routes :: ScottyM ()
routes = do
  S.get "/denied" $ S.text "login denied -- wrong username or password"
  S.get "/login" $ do S.html $ T.pack $ unlines $
                        [ "<form method=\"POST\" action=\"/login\">"
                        , "<input type=\"text\" name=\"username\">"
                        , "<input type=\"password\" name=\"password\">"
                        , "<input type=\"submit\" name=\"login\" value=\"login\">"
                        , "</form>" ]
                      S.post "/login" $ do
                        (usn :: String) <- param "username"
                        (pass :: String) <- param "password"
                        if usn == "guest" && pass == "password"
                          then do addSession conf
                                  redirect "/authed"
                          else do redirect "/denied"
                      S.get "/authcheck" $ authCheck (redirect "/denied") $
                        S.text "authorized"
