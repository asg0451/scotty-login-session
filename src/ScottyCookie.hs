{-
** Code stolen from hackage package scotty-cookie to enable compilation with stack build tool. Code may have been modified as well

Copyright (c) 2014, Mārtiņš Mačs

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Mārtiņš Mačs nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}



{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScottyCookie
       ( makeSimpleCookie
       , setCookie
       , setSimpleCookie
       , setSimpleCookieExpr
       , getCookie
       , getCookies
       , deleteCookie
       ) where

import           Control.Monad            (liftM)

import qualified Data.Text                as TS
import qualified Data.Text.Encoding       as TS
import qualified Data.Text.Lazy.Encoding  as TL

import qualified Data.Map                 as Map

import qualified Data.ByteString.Lazy     as BSL

import           Data.Time.Clock
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)

import           Blaze.ByteString.Builder (toLazyByteString)

import           Web.Cookie
import           Web.Scotty.Trans


makeSimpleCookie :: TS.Text -- ^ name
                 -> TS.Text -- ^ value
                 -> SetCookie
makeSimpleCookie n v = def { setCookieName  = TS.encodeUtf8 n
                           , setCookieValue = TS.encodeUtf8 v
                           }


setCookie :: (Monad m, ScottyError e)
             => SetCookie
             -> ActionT e m ()
setCookie c = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)


-- | 'makeSimpleCookie' and 'setCookie' combined.
setSimpleCookie :: (Monad m, ScottyError e)
                => TS.Text -- ^ name
                -> TS.Text -- ^ value
                -> ActionT e m ()
setSimpleCookie n v = setCookie $ makeSimpleCookie n v

setSimpleCookieExpr :: (Monad m, ScottyError e)
                => TS.Text -- ^ name
                -> TS.Text -- ^ value
                -> UTCTime
                -> ActionT e m ()
setSimpleCookieExpr n v t = setCookie $
                            ((makeSimpleCookie n v) { setCookieExpires = (Just t)})



getCookie :: (Monad m, ScottyError e)
          => TS.Text -- ^ name
          -> ActionT e m (Maybe TS.Text)
getCookie c = liftM (Map.lookup c) getCookies


-- | Returns all cookies
getCookies :: (Monad m, ScottyError e)
              => ActionT e m (Map.Map TS.Text TS.Text)
getCookies = liftM (Map.fromList . maybe [] parse) $ header "Cookie"
  where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8


deleteCookie :: (Monad m, ScottyError e)
             => TS.Text -- ^ name
             -> ActionT e m ()
deleteCookie c = setCookie $ (makeSimpleCookie c "") { setCookieExpires = Just $ posixSecondsToUTCTime 0 }
