{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.OAuth2 where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "binary" Data.Binary qualified as Binary
import "bytestring" Data.ByteString (ByteString)
import "base64-bytestring" Data.ByteString.Base64.URL qualified as Base64
import "bytestring" Data.ByteString.Lazy qualified as BSL
import "base" Data.Kind (Type)
import "base" Data.List qualified as List
import "text" Data.Text (Text, intercalate)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import "base" GHC.Generics (Generic)
import "http-types" Network.HTTP.Types (
  Status (Status),
  hCookie,
  status200,
 )
import "hoauth2" Network.OAuth.OAuth2 qualified as OA2
import "wai" Network.Wai (Request)
import "wai" Network.Wai qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2 qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth.Provider qualified as Wai
import "servant-server" Servant (
  Handler,
  Header,
  Headers,
  NoContent (NoContent),
  StdMethod (GET),
  UVerb,
  Union,
  WithStatus (WithStatus),
  addHeader,
  err401,
  err403,
  err501,
  respond,
  throwError,
  type (:>),
 )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import "servant-server" Servant.Server.Experimental.Auth (
  AuthHandler,
  mkAuthHandler,
 )
import "servant-server" Servant.Server.Generic (
  AsServerT,
 )
import "uri-bytestring" URI.ByteString qualified as U
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


-- | This is the result of successful completion of the OAuth2 login workflow;
-- it is the identifier that comes back from the provider.
type Ident = ByteString


-- | Helpful aliases.
type RedirectWithCookie = Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent


redirectWithCookie ::
  Text ->
  SetCookie ->
  RedirectWithCookie
redirectWithCookie destination cookie =
  addHeader destination (addHeader cookie NoContent)


data OAuth2Routes (rs :: [Type]) mode = AuthRoutes
  { complete :: mode :- "complete" :> UVerb 'GET '[HTML] rs
  }
  deriving stock (Generic)


authServer ::
  forall (rs :: [Type]).
  Union rs ->
  OAuth2Routes rs (AsServerT Handler)
authServer h =
  AuthRoutes
    { complete = pure h
    }


oauth2AuthHandler ::
  forall p rs.
  (Wai.AuthProvider p) =>
  OAuth2Settings p rs ->
  AuthHandler Request (Union rs)
oauth2AuthHandler settings = mkAuthHandler f
 where
  onSuccess ident = pure $ Wai.responseLBS status200 [("", ident)] ""
  onFailure status reason = pure $ Wai.responseLBS status [("", reason)] ""
  f :: Request -> Handler (Union rs)
  f req = do
    resp <- runOAuth2 req (provider settings) onSuccess onFailure
    let thing = snd . head $ Wai.responseHeaders resp
    case Wai.responseStatus resp of
      Status 200 _ -> success settings $ thing
      Status 401 _ -> throwError err401
      Status 403 _ -> throwError err403
      Status 501 _ -> throwError err501
      _ -> error $ "Unknown error: " <> show thing


-- | An extremely unfortunate way of getting the redirect URL; stolen from
-- 'Network.Wai.Auth.Internal'.
getRedirectUrl :: Text -> Wai.OAuth2 -> Maybe [Text] -> Text
getRedirectUrl callbackUrl waiOa2 oa2Scope = decodeUtf8 redirectUrl
 where
  scope = (encodeUtf8 . intercalate " ") <$> oa2Scope
  redirectUrl =
    getRedirectURI $
      (flip OA2.appendQueryParams)
        (OA2.authorizationUrl oa2)
        (maybe [] ((: []) . ("scope",)) scope)
  oa2 = maybe (error "Couldn't construct the OAuth2 record.") id fromInteralOAuth2
  getRedirectURI = U.serializeURIRef'
  parseAbsoluteURI urlTxt = do
    case U.parseURI U.strictURIParserOptions (encodeUtf8 urlTxt) of
      Left _ -> Nothing
      Right url -> pure url
  fromInteralOAuth2 = do
    authEndpointURI <- parseAbsoluteURI $ Wai.oa2AuthorizeEndpoint waiOa2
    callbackURI <- parseAbsoluteURI callbackUrl
    pure $
      OA2.OAuth2
        { OA2.oauth2ClientId = Wai.oa2ClientId waiOa2
        , OA2.oauth2ClientSecret = error "Client secret not needed."
        , OA2.oauth2AuthorizeEndpoint = authEndpointURI
        , OA2.oauth2TokenEndpoint = error "No token endpoint"
        , OA2.oauth2RedirectUri = callbackURI
        }


-- | In the context of Wai, run the 'complete' step of the OAuth2 process. We
-- return a Wai.Response. We will interpret this later into Servant responses.
runOAuth2 ::
  (MonadIO m, Wai.AuthProvider p) =>
  Request ->
  p ->
  (Wai.AuthLoginState -> IO Wai.Response) ->
  (Status -> ByteString -> IO Wai.Response) ->
  m Wai.Response
runOAuth2 request p onSuccess onFailure = do
  let appRoot = Wai.smartAppRoot request
      suffix = ["complete"]
      provider = Wai.Provider p
      providerUrl (Wai.ProviderUrl url) = Wai.mkRouteRender (Just appRoot) "auth" url provider
  liftIO $ Wai.handleLogin provider request suffix providerUrl onSuccess onFailure


data OAuth2Settings p (rs :: [Type]) = OAuth2Settings
  { success :: Ident -> Handler (Union rs)
  , provider :: p
  }


defaultOAuth2Settings :: p -> OAuth2Settings p '[WithStatus 200 Text]
defaultOAuth2Settings p =
  OAuth2Settings
    { success = respond . WithStatus @200 . decodeUtf8
    , provider = p
    }


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
