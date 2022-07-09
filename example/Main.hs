{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Request)
import "warp" Network.Wai.Handler.Warp (run)
import "servant-server" Servant
  ( AuthProtect,
    Context (EmptyContext, (:.)),
    Get,
    Handler,
    NamedRoutes,
    type (:>),
  )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import Servant.OAuth2
import "servant-server" Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
  )
import "servant-server" Servant.Server.Generic
  ( AsServerT,
    genericServeTWithContext,
  )
import "shakespeare" Text.Hamlet (Html, shamlet)
import "cookie" Web.Cookie (SetCookie)
import Data.Text.Encoding (decodeUtf8)

type PageM = Handler

data Session = Session
  { ident :: Text
  }

data Routes mode = Routes
  { home :: mode :- Get '[HTML] Html,
    admin :: mode :- "admin" :> NamedRoutes AdminRoutes,
    auth :: mode :- AuthProtect "oauth2" :> NamedRoutes (OAuth2Routes OAuth2Result)
  }
  deriving stock (Generic)

data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)

type OAuth2Result = Text
settings = defaultOAuth2Settings { success = pure . decodeUtf8 }

server :: Routes (AsServerT PageM)
server =
  Routes
    { home = pure $ [shamlet| <p> Home |],
      admin = adminServer,
      auth = authServer @OAuth2Result settings
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
    -- context :: Context '[AuthHandler Request SetCookie]
    context = oauth2AuthHandler :. EmptyContext
    nat = id
