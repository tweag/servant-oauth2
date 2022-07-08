{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Request)
import "warp" Network.Wai.Handler.Warp (run)
import "servant-server" Servant (Context (EmptyContext, (:.)), Get, Handler, NamedRoutes, type (:>))
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import "servant-server" Servant.Server.Generic (AsServerT, genericServeTWithContext)
import "shakespeare" Text.Hamlet (Html, shamlet)

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
  , auth :: mode :- OAuth2Provider GitHubProvider :> NamedRoutes AuthRoutes
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


server :: Routes (AsServerT PageM)
server =
  Routes
    { home = pure $ [shamlet| <p> Home |]
    , admin = adminServer
    , auth = undefined
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
