{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Admin where

import "base" GHC.Generics (Generic)
import "servant-server" Servant
  ( Get,
  )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import "servant-server" Servant.Server.Generic
  ( AsServerT,
  )
import "shakespeare" Text.Hamlet (Html, shamlet)
import Types

data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)

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
