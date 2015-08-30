module Web.Scotty.Login.Internal.Cookies where
import qualified Data.Text         as T
import           Data.Time.Clock
import           Web.Cookie
import           Web.Scotty.Cookie
import           Web.Scotty.Trans

setSimpleCookieExpr :: (Monad m, ScottyError e)
                => T.Text
                -> T.Text
                -> UTCTime
                -> ActionT e m ()
setSimpleCookieExpr n v t = setCookie
                              ((makeSimpleCookie n v) { setCookieExpires = Just t})
