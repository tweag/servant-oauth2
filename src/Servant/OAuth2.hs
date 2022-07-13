{-# language NamedFieldPuns #-}
{-# language TypeFamilies   #-}

module Servant.OAuth2 where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "bytestring" Data.ByteString (ByteString)
import "base" Data.Kind (Type)
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8)
import "base" GHC.Generics (Generic)
import "http-types" Network.HTTP.Types (
  Status (Status),
  status200,
 )
import "wai" Network.Wai (Request)
import "wai" Network.Wai qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth.Provider qualified as Wai
import "servant-server" Servant (
  Handler,
  StdMethod (GET),
  UVerb,
  Union,
  WithStatus (WithStatus),
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


data Tag a b = Tag { unTag :: b }


-- | This is the result of successful completion of the OAuth2 login workflow;
-- it is the identifier that comes back from the provider.
type Ident = ByteString


data OAuth2Routes (rs :: [Type]) mode = AuthRoutes
  { complete :: mode :- "complete" :> UVerb 'GET '[HTML] rs
  }
  deriving stock (Generic)


authServer ::
  forall m a (rs :: [Type]).
  Monad m =>
  Tag a (Union rs) ->
  OAuth2Routes rs (AsServerT m)
authServer h =
  AuthRoutes
    { complete = pure (unTag h)
    }


oauth2AuthHandler ::
  forall p rs.
  (Wai.AuthProvider p) =>
  OAuth2Settings p rs ->
  AuthHandler Request (Tag p (Union rs))
oauth2AuthHandler settings = mkAuthHandler f
 where
  onSuccess ident = pure $ Wai.responseLBS status200 [("", ident)] ""
  onFailure status reason = pure $ Wai.responseLBS status [("", reason)] ""
  f :: Request -> Handler (Tag p (Union rs))
  f req = do
    resp <- runOAuth2 req (provider settings) onSuccess onFailure
    let thing = snd . head $ Wai.responseHeaders resp
    case Wai.responseStatus resp of
      Status 200 _ -> fmap Tag $ success settings $ thing
      Status 401 _ -> throwError err401
      Status 403 _ -> throwError err403
      Status 501 _ -> throwError err501
      _ -> error $ "Unknown error: " <> show thing


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
