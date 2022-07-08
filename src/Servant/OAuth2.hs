{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.OAuth2 where

import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Kind (Type)
import "base" Data.Typeable (Typeable)
import "wai" Network.Wai (Request)
import "servant-server" Servant (Handler, HasContextEntry, HasServer (..), Proxy (Proxy), type (:>))
import "servant-server" Servant.Server.Internal (DelayedIO, addAuthCheck, delayedFailFatal, getContextEntry, runHandler, withRequest)


data GitHubProvider
data GoogleProvider
data GitLabProvider


data OAuth2Provider (tag :: k) deriving (Typeable)


type family OAuth2ProviderData (a :: Type)


newtype OAuth2Handler r usr = OAuth2Handler
  { unOAuth2Handler :: r -> Handler usr
  }


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
