{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.SLSSpec(main, spec) where
import           Control.Concurrent        (forkIO, killThread)
import           Control.Exception
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (unpack)
import           Data.CaseInsensitive      (CI)
import qualified Data.CaseInsensitive      as CI
import           Data.List                 (isPrefixOf)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as T
import           Network.HTTP
import           Network.HTTP.Types.Header
import           Network.Wai.Test
import           System.Directory
import           System.Exit               (exitFailure)
import           Test.Hspec
import           Test.Hspec.Wai            as W
import           Test.Hspec.Wai.Internal
import           Web.Scotty                as S
import           Web.Scotty.Cookie         as C
import           Web.Scotty.Login.Session

testPort = 4040
baseURL = "localhost" ++ ":" ++ show testPort

main = do
  catch (void $ removeFile dbName) (\(e :: SomeException) -> return ())
  hspec spec
  catch (void $ removeFile dbName) (\(e :: SomeException) -> return ())


spec :: Spec
spec = do
  describe "baseline" $ do
    withApp routes $ do
      it "GET to root returns 200 code" $ do
        W.get "/" `shouldRespondWith` 200

  describe "denial" $ do
    withApp routes $ do
      it "denies if not authed" $ do
        W.get "/authcheck" `shouldRespondWith` "denied" {matchStatus = 403}

  describe "login" $ do
    withApp routes $ do
      it "logs in successfully" $
        W.postHtmlForm "/login" [("username", "guest"), ("password", "password")] `shouldRespondWith` "authed"

  describe "auth usage" $ do
    withApp routes $ do
      it "gives sessionID cookie when i log in" $ do
        resp <- W.postHtmlForm "/login" [("username", "guest"), ("password", "password")]
        let hs = simpleHeaders resp
            c  = lookup "Set-Cookie" hs
        -- honestly i am shocked that this works
        liftIO $ c `shouldSatisfy` \case Nothing -> False
                                         Just s -> "SessionId=" `isPrefixOf` unpack s
      it "allows access to page" $ do
        resp <- W.postHtmlForm "/login" [("username", "guest"), ("password", "password")]
        let hs = simpleHeaders resp
            c  = fromMaybe "" $ lookup "Set-Cookie" hs
            headers = [ ("Cookie",  c) ]
        W.request "GET" "/authcheck" headers "" `shouldRespondWith` "authorized"

      it "forbids access to page" $ do
        resp <- W.postHtmlForm "/login" [("username", "guest"), ("password", "password")]
        W.get "/authcheck" `shouldRespondWith` "denied" {matchStatus = 403}



-- withApp :: ScottyM () -> SpecWith Application -> Spec
-- like before_each
withApp r = with (initializeCookieDb conf >> scottyApp r)


----- basic library usage
dbName = "__test__.sqlite3"

conf :: SessionConfig
conf = defaultSessionConfig { dbPath = dbName }

routes :: ScottyM ()
routes = do
  S.get "/" $ S.text "howdy"
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
              S.html "authed"
      else do S.html "denied"
  S.get "/authcheck" $ authCheck (S.html "denied") $
    S.html "authorized"
