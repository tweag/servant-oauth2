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


-- | Helpful aliases.
type RedirectWithCookie = Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent


redirectWithCookie :: Text -> SetCookie -> RedirectWithCookie
redirectWithCookie destination cookie =
  addHeader destination (addHeader cookie NoContent)


-- | Build a simple cook provided you have a function that can convert the
-- ident into a sessionId kind of object.
simpleCookieOAuth2Settings ::
  Binary.Binary s =>
  p ->
  (Ident -> Handler s) ->
  Key ->
  OAuth2Settings p '[WithStatus 303 RedirectWithCookie]
simpleCookieOAuth2Settings p toSessionId key =
  (defaultOAuth2Settings p)
    { success = \ident -> do
        sid <- toSessionId ident
        cookie <- liftIO $ buildSessionCookie key sid
        respond $ WithStatus @303 (redirectWithCookie "/" cookie)
    }


ourCookie :: ByteString
ourCookie = "_servant_oauth2_cookie"


-- | Make a session cookie from the ident; i.e. just set the cookie to be the
-- ident value.
buildSessionCookie :: Binary.Binary s => Key -> s -> IO SetCookie
buildSessionCookie key sid = do
  encrypted <- encryptIO key $ BSL.toStrict $ Binary.encode $ sid
  pure $
    -- Todo: Allow people to configure the cookie itself.
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
