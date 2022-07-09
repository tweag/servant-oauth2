{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Admin
import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "warp" Network.Wai.Handler.Warp (run)
import "servant-server" Servant
  ( AuthProtect,
    Context (EmptyContext, (:.)),
    Get,
    NamedRoutes,
    type (:>),
  )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import Servant.OAuth2
import "servant-server" Servant.Server.Generic
  ( AsServerT,
    genericServeTWithContext,
  )
import "shakespeare" Text.Hamlet (Html, shamlet)
import Types

-- import "cookie" Web.Cookie (SetCookie)

data Routes mode = Routes
  { home :: mode :- Get '[HTML] Html,
    admin :: mode :- "admin" :> NamedRoutes AdminRoutes,
    auth :: mode :- AuthProtect "oauth2" :> NamedRoutes (OAuth2Routes OAuth2Result)
  }
  deriving stock (Generic)

type OAuth2Result = Text

settings :: OAuth2Settings Text
settings =
  defaultOAuth2Settings

server :: Routes (AsServerT PageM)
server =
  Routes
    { home = pure $ [shamlet| <p> Home |],
      admin = adminServer,
      auth = authServer settings
    }

main :: IO ()
main = do
  run 8080 $
    genericServeTWithContext nat server context
  where
    context = oauth2AuthHandler settings :. EmptyContext
    nat = id
