{-|
A modern wrapper around <https://github.com/fpco/wai-middleware-auth/>.

This library can be used to add type-level authentication and authorisation
into a Servant api via OAuth2. You might be interested in this if you are
using Servant as a light-weight webserver serving 'Html', for example.
-}

{-# language TypeFamilies #-}

module Servant.OAuth2 where

import "exceptions" Control.Monad.Catch (MonadThrow (..))
import "mtl" Control.Monad.Except (MonadError)
import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "bytestring" Data.ByteString (ByteString)
import "base" Data.Kind (Type)
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8)
import "base" GHC.Generics (Generic)
import "http-types" Network.HTTP.Types
  ( Status (Status)
  , status200
  )
import "wai" Network.Wai (Request)
import "wai" Network.Wai qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth.Provider qualified as Wai
import "servant-server" Servant
  ( Handler
  , StdMethod (GET)
  , UVerb
  , Union
  , WithStatus (WithStatus)
  , err401
  , err403
  , err501
  , respond
  , type (:>)
  )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import "servant-server" Servant.Server.Experimental.Auth
  ( AuthHandler
  , mkAuthHandler
  )
import "servant-server" Servant.Server.Generic
  ( AsServerT
  )


-- | A simple way to add a type-level tag onto the return type for your
-- 'AuthServerData' instance. Used something like: @'Tag' 'Github' ...@; this then
-- allows you to use multiple oauth providers on one server, and have servant
-- still pick out the right auth handler to use. See:
-- <https://docs.servant.dev/en/stable/tutorial/Authentication.html#recap> for
-- more detail.
--
-- @since 0.1.0.0
newtype Tag a (rs :: [Type]) = Tag { unTag :: Union rs }


-- | This is the result of successful completion of the OAuth2 login workflow;
-- it is the identifier that comes back from the provider.
--
-- @since 0.1.0.0
type Ident = ByteString


-- | This contains the 'complete' route that the given OAuth2 provider will
-- return to. This implementation for this is fully given by the 'authServer'
-- function.
--
-- @since 0.1.0.0
data OAuth2Routes (rs :: [Type]) mode = AuthRoutes
  { complete :: mode :- "complete" :> UVerb 'GET '[HTML] rs
  }
  deriving stock (Generic)


-- | The server implementation for the 'OAuth2Routes' routes. Ultimately,
-- this just returns the result of the 'success' function from
--'OAuth2Settings'.
--
-- @since 0.1.0.0
authServer :: forall m a (rs :: [Type])
   .  Monad m
  => Tag a rs
  -> OAuth2Routes rs (AsServerT m)
authServer h =
  AuthRoutes
    { complete = pure (unTag h)
    }


-- | The central handler that runs when the 'complete' route is called. In
-- here we pass of to 'Wai.handleLogin' via 'runOAuth2', and we unwrap the
-- results; if there was any error, we throw a servant error, or, in the happy
-- case that we successfully authenticate, we call the 'success' function and
-- return, which is then (after unwrapping) returned by the 'authServer'.
--
-- @since 0.1.0.0
oauth2AuthHandler :: forall m p rs e
   . ( Wai.AuthProvider p
     , MonadIO m
     , MonadThrow m
     , MonadError e m
     , Monad m
     )
  => OAuth2Settings m p rs
  -> (m (Tag p rs) -> Handler (Tag p rs))
  -> AuthHandler Request (Tag p rs)
oauth2AuthHandler settings runM = mkAuthHandler $ runM . f
 where
  onSuccess ident = pure $ Wai.responseLBS status200 [("", ident)] ""
  onFailure status reason = pure $ Wai.responseLBS status [("", reason)] ""
  f :: Request -> m (Tag p rs)
  f req = do
    resp <- runOAuth2 req (provider settings) onSuccess onFailure
    let thing = snd . head $ Wai.responseHeaders resp
    case Wai.responseStatus resp of
      Status 200 _ -> Tag <$> success settings req thing
      Status 401 _ -> throwM err401
      Status 403 _ -> throwM err403
      Status 501 _ -> throwM err501
      _ -> error $ "Unknown error: " <> show thing


-- | In the context of Wai, run the 'complete' step of the OAuth2 process. We
-- return a 'Wai.Response', unfortunately, which we will later interpret into
-- Servant responses.
--
-- @since 0.1.0.0
runOAuth2 :: (MonadIO m, Wai.AuthProvider p)
  => Request
  -> p
  -> (Wai.AuthLoginState -> IO Wai.Response)
  -> (Status -> ByteString -> IO Wai.Response)
  -> m Wai.Response
runOAuth2 request p onSuccess onFailure = do
  let appRoot = Wai.smartAppRoot request
      suffix  = ["complete"]
      p'      = Wai.Provider p
      providerUrl (Wai.ProviderUrl url) = Wai.mkRouteRender (Just appRoot) "auth" url p'
  liftIO $ Wai.handleLogin p' request suffix providerUrl onSuccess onFailure


-- | Used to record the particular provider you are using, along with the
-- ultimate return type of the 'complete' route, that will, in the end, need
-- to agree with the particular implementation of the 'success' function.
--
-- @since 0.1.0.0
data OAuth2Settings m p (rs :: [Type]) = OAuth2Settings
  { success  :: Request -> Ident -> m (Union rs)
  , provider :: p
  }


-- | Default settings, only really suitable for demo purposes, that simply
-- always respond with just the return value of the OAuth2 route; typically
-- the email, depending on how your provider is configured.
--
-- Note that in order to use this, your instance of 'AuthServerData' must
-- return @\'['WithStatus' 200 'Text']@.
--
-- @since 0.1.0.0
defaultOAuth2Settings :: forall m p
   . Applicative m
  => p
  -> OAuth2Settings m p '[WithStatus 200 Text]
defaultOAuth2Settings p =
  OAuth2Settings
    { success  = \_ -> respond . WithStatus @200 . decodeUtf8
    , provider = p
    }
