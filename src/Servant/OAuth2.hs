{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.OAuth2 where

import "base" Control.Monad.IO.Class (liftIO)
import "bytestring" Data.ByteString
import "base" Data.Kind (Type)
import "text" Data.Text (Text)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Request)
import "servant-server" Servant
  ( AuthProtect,
    Get,
    Handler,
    HasContextEntry,
    HasServer (..),
    Header,
    Headers,
    NoContent (NoContent),
    Proxy (Proxy),
    addHeader,
    type (:>),
  )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import "servant-server" Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
  )
import "servant-server" Servant.Server.Generic
  ( AsServerT,
  )
import "servant-server" Servant.Server.Internal
  ( DelayedIO,
    addAuthCheck,
    delayedFailFatal,
    getContextEntry,
    runHandler,
    withRequest,
  )
import "cookie" Web.Cookie (SetCookie)

type Ident = ByteString

-- | We begin
type instance AuthServerData (AuthProtect "oauth2") = Ident

-- | Then, let's begin.
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
  { complete :: mode :- Get '[HTML] a
  }
  deriving stock (Generic)

authServer :: forall a. OAuth2Settings a -> Ident -> OAuth2Routes a (AsServerT Handler)
authServer settings ident =
  AuthRoutes
    { complete = success settings $ ident
    }

oauth2AuthHandler :: AuthHandler Request Ident
oauth2AuthHandler = undefined

data OAuth2Settings a = OAuth2Settings
  { success :: Ident -> Handler a
  }

defaultOAuth2Settings :: OAuth2Settings RedirectWithCookie
defaultOAuth2Settings =
  OAuth2Settings
    { success = \ident -> do
        cookie <- buildSessionCookie key ident
        pure $ redirectWithCookie "/" cookie
    }
  where
    key = undefined

buildSessionCookie = undefined
