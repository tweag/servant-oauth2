{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- Todo:
--  - [ ] Make sure the right url is picked up in the callback; "/github", or
--  something.
module Servant.OAuth2 where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "binary" Data.Binary qualified as Binary
import "bytestring" Data.ByteString (ByteString)
import "base64-bytestring" Data.ByteString.Base64.URL qualified as Base64
import "bytestring" Data.ByteString.Lazy qualified as BSL
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8)
import "base" GHC.Generics (Generic)
import "http-types" Network.HTTP.Types
  ( Status (Status),
    status200,
  )
import "wai" Network.Wai (Request)
import "wai" Network.Wai qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Github (mkGithubProvider)
import "wai-middleware-auth" Network.Wai.Middleware.Auth.Provider qualified as Wai
import "servant-server" Servant
  ( AuthProtect,
    Get,
    Handler,
    Header,
    Headers,
    NoContent (NoContent),
    addHeader,
    err401,
    err403,
    err501,
    throwError,
    type (:>),
  )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import "servant-server" Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
    mkAuthHandler,
  )
import "servant-server" Servant.Server.Generic
  ( AsServerT,
  )
import "clientsession" Web.ClientSession
  ( Key,
    decrypt,
    encryptIO,
  )
import "cookie" Web.Cookie
  ( SetCookie (..),
    defaultSetCookie,
    sameSiteStrict,
  )

-- | This is the result of successful completion of the OAuth2 login workflow;
-- it is the identifier that comes back from the provider.
type Ident = ByteString

-- | The handler will return the Ident, that can then be proessed by the
-- 'success' function of 'OAuth2Settings'.
type instance AuthServerData (AuthProtect "oauth2") = Ident

-- | Helpful aliases.
type Redirect =
  Headers '[Header "Location" Text] NoContent

redirect :: Text -> Redirect
redirect location = addHeader location NoContent

type RedirectWithCookie =
  Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent

redirectWithCookie ::
  Text ->
  SetCookie ->
  RedirectWithCookie
redirectWithCookie destination cookie =
  addHeader destination (addHeader cookie NoContent)

data OAuth2Routes a mode = AuthRoutes
  { complete :: mode :- "auth" :> Get '[HTML] a
  }
  deriving stock (Generic)

authServer :: forall a. OAuth2Settings a -> Ident -> OAuth2Routes a (AsServerT Handler)
authServer settings ident =
  AuthRoutes
    { complete = success settings $ ident
    }

oauth2AuthHandler :: forall a. OAuth2Settings a -> AuthHandler Request Ident
oauth2AuthHandler settings = mkAuthHandler f
  where
    onSuccess ident = pure $ Wai.responseLBS status200 [("", ident)] ""
    onFailure status reason = pure $ Wai.responseLBS status [("", reason)] ""
    f :: Request -> Handler Ident
    f req = do
      resp <- runOAuth2 req settings onSuccess onFailure
      let thing = snd . head $ Wai.responseHeaders resp
      case Wai.responseStatus resp of
        Status 200 _ -> pure thing
        Status 401 _ -> throwError err401
        Status 403 _ -> throwError err403
        Status 501 _ -> throwError err501
        _ -> error "Unknown error."

-- | In the context of Wai, run the 'complete' step of the OAuth2 process. We
-- return a Wai.Response. We will interpret this later into Servant responses.
runOAuth2 ::
  (MonadIO m) =>
  Request ->
  OAuth2Settings a ->
  (Wai.AuthLoginState -> IO Wai.Response) ->
  (Status -> ByteString -> IO Wai.Response) ->
  m Wai.Response
runOAuth2 request OAuth2Settings {appSecret, appClientId, appName, appEmailAllowList} onSuccess onFailure = do
  let appRoot = Wai.smartAppRoot request
      provider = Wai.Provider $ mkGithubProvider appName appClientId appSecret appEmailAllowList Nothing
      suffix = ["complete"]
      providerUrl (Wai.ProviderUrl url) = Wai.mkRouteRender (Just appRoot) "auth" url provider
  liftIO $ Wai.handleLogin provider request suffix providerUrl onSuccess onFailure

data OAuth2Settings a = OAuth2Settings
  { success :: Ident -> Handler a,
    appSecret :: Text,
    appClientId :: Text,
    appName :: Text,
    appEmailAllowList :: [ByteString]
  }

defaultOAuth2Settings :: OAuth2Settings Text
defaultOAuth2Settings =
  OAuth2Settings
    { success = pure . decodeUtf8,
      appSecret = "",
      appClientId = "",
      appName = "",
      appEmailAllowList = [".*"]
    }

-- | Build a simple cook provided you have a function that can convert the
-- ident into a sessionId kind of object.
simpleCookieOAuth2Settings ::
  Binary.Binary s =>
  (Ident -> Handler s) ->
  Key ->
  OAuth2Settings RedirectWithCookie
simpleCookieOAuth2Settings toSessionId key =
  defaultOAuth2Settings
    { success = \ident -> do
        sid <- toSessionId ident
        cookie <- liftIO $ buildSessionCookie key sid
        pure $ redirectWithCookie "/" cookie
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
      { setCookieName = ourCookie,
        setCookieValue = Base64.encode encrypted,
        setCookieMaxAge = oneWeek,
        setCookiePath = Just "/",
        setCookieSameSite = Just sameSiteStrict,
        setCookieHttpOnly = True,
        setCookieSecure = False
      }
  where
    oneWeek = Just $ 3600 * 24 * 7
