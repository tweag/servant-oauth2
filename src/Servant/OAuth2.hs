{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.OAuth2 where

import "text" Data.Text (Text)
import "bytestring" Data.ByteString (ByteString)
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Kind (Type)
import "base" Data.Typeable (Typeable)
import "wai" Network.Wai (Request)
import "servant-server" Servant (
  Handler,
  HasContextEntry,
  HasServer (..),
  Proxy (Proxy),
  type (:>),
  throwError,
  err401,
 )
import "servant-server" Servant.Server.Internal (
  DelayedIO,
  addAuthCheck,
  delayedFailFatal,
  getContextEntry,
  runHandler,
  withRequest,
 )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.Provider qualified as Wai
import "wai" Network.Wai qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth qualified as Wai
import "http-types" Network.HTTP.Types (Status, status200, hCookie)


data GitHubProvider
data GoogleProvider
data GitLabProvider


data OAuth2Provider (tag :: k) deriving (Typeable)


type family OAuth2ProviderData (a :: Type)


newtype OAuth2Handler r usr = OAuth2Handler
  { unOAuth2Handler :: r -> Handler usr
  }


mkOAuth2Handler :: (r -> Handler usr) -> OAuth2Handler r usr
mkOAuth2Handler = OAuth2Handler


instance
  ( HasServer api context
  , HasContextEntry context (OAuth2Handler Request (OAuth2ProviderData (OAuth2Provider tag)))
  ) =>
  HasServer (OAuth2Provider tag :> api) context
  where
  type
    ServerT (OAuth2Provider tag :> api) m =
      OAuth2ProviderData (OAuth2Provider tag) -> ServerT api m


  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s


  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler :: Request -> Handler (OAuth2ProviderData (OAuth2Provider tag))
      authHandler = unOAuth2Handler (getContextEntry context)

      authCheck :: Request -> DelayedIO (OAuth2ProviderData (OAuth2Provider tag))
      authCheck = (>>= either delayedFailFatal return) . liftIO . runHandler . authHandler


data OAuth2Settings = OAuth2Settings
  { clientId :: Text
  , clientSecret :: Text
  }


data Settings a = Settings
  { identHandler :: ByteString -> Handler a
  , oauthSettings :: OAuth2Settings
  }


completeAuthHandler :: Settings a -> OAuth2Handler Request a
completeAuthHandler settings = mkOAuth2Handler handler
  where
    handler :: Request -> Handler a
    handler request = do
      let success ident = pure $ Wai.responseLBS status200 [("Success", ident)] ""
          failure resultStatus x = pure $ Wai.responseLBS resultStatus [("Failure", x)] ""

      response <- undefined -- runGithubAuth request (_oauth (config env)) (Just success) (Just failure) DoComplete

      let headers = Wai.responseHeaders response

      case lookup "Success" headers of
        Nothing -> throwError err401
        Just ident -> do
          -- We're in!
          let key = undefined
          undefined
          -- cookie <- liftIO $ buildSessionCookie key ident
          -- pure (Complete cookie)
