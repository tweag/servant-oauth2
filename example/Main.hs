{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Request)
import "warp" Network.Wai.Handler.Warp (run)
import "servant-server" Servant (
  Context (EmptyContext, (:.)),
  Get,
  Handler,
  Header,
  Headers,
  NamedRoutes,
  NoContent (NoContent),
  type (:>),
 )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import "servant-server" Servant.Server.Generic (AsServerT, genericServeTWithContext)
import "shakespeare" Text.Hamlet (Html, shamlet)
import "cookie" Web.Cookie (SetCookie)

import Servant.OAuth2


type PageM = Handler


data Session = Session
  { ident :: Text
  }


type instance OAuth2ProviderData (OAuth2Provider GitHubProvider) = Session


f :: OAuth2Handler Request Session
f = undefined


data Routes mode = Routes
  { home :: mode :- Get '[HTML] Html
  , admin :: mode :- "admin" :> NamedRoutes AdminRoutes
  , auth :: mode :- OAuth2Provider GitHubProvider :> Get '[HTML] RedirectWithCookie
  }
  deriving stock (Generic)


data AuthRoutes mode = AuthRoutes
  { complete :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)


data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)


type Redirect =
  Headers '[Header "Location" Text] NoContent


type RedirectWithCookie =
  Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent


authHandler :: Session -> Handler RedirectWithCookie
authHandler = undefined


server :: Routes (AsServerT PageM)
server =
  Routes
    { home = pure $ [shamlet| <p> Home |]
    , admin = adminServer
    , auth = authHandler
    }


adminServer :: AdminRoutes (AsServerT PageM)
adminServer =
  AdminRoutes
    { adminHome =
        pure $
          [shamlet|
        <p> Admin
        <i> Top secrets secrets ...
      |]
    }


main :: IO ()
main = do
  run 8080 $
    genericServeTWithContext nat server context
  where
    context = f :. EmptyContext
    nat = id
