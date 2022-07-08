{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant (Context (EmptyContext), Get, Handler, type (:>))
import Servant.API.Generic ((:-))
import Servant.HTML.Blaze (HTML)
import Servant.Server.Generic (AsServerT, genericServeTWithContext)
import Text.Hamlet (Html, shamlet)

type PageM = Handler

data Api mode = Api
  { home :: mode :- Get '[HTML] Html
  , about :: mode :- "about" :> Get '[HTML] Html
  }
  deriving stock (Generic)

server :: Api (AsServerT PageM)
server =
  Api
    { home = pure $ [shamlet| <p> Home |]
    , about = pure $ [shamlet| <p> About |]
    }

main :: IO ()
main = do
  run 8083 $
    genericServeTWithContext nat server context
  where
    context = EmptyContext
    nat = id
