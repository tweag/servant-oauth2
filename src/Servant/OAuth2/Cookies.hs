module Servant.OAuth2.Cookies where

import "base" Control.Monad.IO.Class (liftIO)
import "binary" Data.Binary qualified as Binary
import "bytestring" Data.ByteString (ByteString)
import "base64-bytestring" Data.ByteString.Base64.URL qualified as Base64
import "bytestring" Data.ByteString.Lazy qualified as BSL
import "base" Data.List qualified as List
import "text" Data.Text (Text)
import "http-types" Network.HTTP.Types (
  hCookie,
 )
import "wai" Network.Wai (Request)
import "wai" Network.Wai qualified as Wai
import "servant-server" Servant (
  Handler,
  Header,
  Headers,
  NoContent (NoContent),
  WithStatus (WithStatus),
  addHeader,
  respond,
 )
import Servant.OAuth2
import "clientsession" Web.ClientSession (
  Key,
  decrypt,
  encryptIO,
 )
import "cookie" Web.Cookie (
  SetCookie (..),
  defaultSetCookie,
  parseCookies,
  sameSiteStrict,
 )


-- | A helpful alias for returning a 'Location' header and a 'SetCookie'
-- header.
--
-- @since 0.1.0.0
type RedirectWithCookie
  = Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent


-- | Default name of our cookie.
--
-- @since 0.1.0.0
ourCookie :: ByteString
ourCookie = "_servant_oauth2_cookie"


-- | Set a cookie and then perform a redirection; typically used as part of
-- logging in; i.e. after successfully performing OAuth2 authentication, we
-- just want to redirect back to the homepage.
--
-- @since 0.1.0.0
redirectWithCookie :: Text -> SetCookie -> RedirectWithCookie
redirectWithCookie destination cookie =
  addHeader destination (addHeader cookie NoContent)


-- | Build a simple cookie provided you have a function that can convert the
-- ident into a sessionId kind of object.
--
-- @since 0.1.0.0
simpleCookieOAuth2Settings :: Binary.Binary s
  => p
  -> (Ident -> Handler s)
  -> Key
  -> OAuth2Settings p '[WithStatus 303 RedirectWithCookie]
simpleCookieOAuth2Settings p toSessionId key =
  (defaultOAuth2Settings p)
    { success = \ident -> do
        sid <- toSessionId ident
        cookie <- liftIO $ buildSessionCookie key sid
        respond $ WithStatus @303 (redirectWithCookie "/" cookie)
    }


-- | Make a session cookie from the ident; i.e. just set the cookie to be the
-- ident value.
--
-- @since 0.1.0.0
buildSessionCookie :: Binary.Binary s => Key -> s -> IO SetCookie
buildSessionCookie key sid = do
  encrypted <- encryptIO key $ BSL.toStrict $ Binary.encode $ sid
  pure $
    defaultSetCookie
      { setCookieName = ourCookie
      , setCookieValue = Base64.encode encrypted
      , setCookieMaxAge = oneWeek
      , setCookiePath = Just "/"
      , setCookieSameSite = Just sameSiteStrict
      , setCookieHttpOnly = True
      , setCookieSecure = False
      }
 where
  oneWeek = Just $ 3600 * 24 * 7


-- | Perform the decryption of the cookie in reverse order to
-- 'buildSessionCookie'.
--
-- @since 0.1.0.0
getSessionIdFromCookie :: Binary.Binary s => Request -> Key -> Maybe s
getSessionIdFromCookie request key = maybeSessionId
 where
  fromEither = either (const Nothing)
  maybeSessionId = do
    cookies <- parseCookies <$> List.lookup hCookie (Wai.requestHeaders request)
    v <- List.lookup ourCookie cookies
    e <- fromEither Just $ Base64.decode v
    x <- decrypt key e
    i <- fromEither (\(_, _, c) -> Just c) $ Binary.decodeOrFail (BSL.fromStrict x)
    pure i
