{-# language NamedFieldPuns  #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}

module Servant.OAuth2.Examples.Cookies where

import "base" Data.Maybe (fromJust, isJust)
import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Request)
import "warp" Network.Wai.Handler.Warp (run)
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Github
  ( Github (..)
  , mkGithubProvider
  )
import "servant-server" Servant
  ( AuthProtect
  , Context (EmptyContext, (:.))
  , Get
  , Handler
  , NamedRoutes
  , WithStatus
  , type (:>)
  )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import Servant.OAuth2
import Servant.OAuth2.Cookies
import Servant.OAuth2.Examples.Config
import Servant.OAuth2.Hacks
import "servant-server" Servant.Server.Experimental.Auth
  ( AuthHandler
  , AuthServerData
  , mkAuthHandler
  )
import "servant-server" Servant.Server.Generic
  ( AsServerT
  , genericServeTWithContext
  )
import "shakespeare" Text.Hamlet (Html, shamlet)
import "tomland" Toml (decodeFileExact)
import "clientsession" Web.ClientSession (Key, getDefaultKey)


type OAuth2Result = '[WithStatus 303 RedirectWithCookie]


type instance AuthServerData (AuthProtect Github) = Tag Github OAuth2Result


-- | This will be the name of the logged in user. We'll just display it.
type instance AuthServerData (AuthProtect "optional-cookie") = Maybe Text


optionalUserAuthHandler :: Key -> AuthHandler Request (Maybe Text)
optionalUserAuthHandler key = mkAuthHandler f
 where
  f :: Request -> Handler (Maybe Text)
  f req = do
    let sessionId = getSessionIdFromCookie req key
    pure sessionId


data Routes mode = Routes
  { home :: mode :- AuthProtect "optional-cookie" :> Get '[HTML] Html
  , auth ::
      mode
        :- AuthProtect Github
          :> "auth"
          :> "github"
          :> NamedRoutes (OAuth2Routes OAuth2Result)
  }
  deriving stock (Generic)


mkSettings :: Key -> OAuthConfig -> OAuth2Settings Github OAuth2Result
mkSettings key c = settings
 where
  toSessionId = pure . id
  provider = mkGithubProvider (_name c) (_id c) (_secret c) emailAllowList Nothing
  settings = simpleCookieOAuth2Settings provider toSessionId key
  emailAllowList = [".*"]


server ::
  OAuthConfig ->
  OAuth2Settings Github OAuth2Result ->
  Routes (AsServerT Handler)
server OAuthConfig {_callbackUrl} settings =
  Routes
    { home = \user -> do
        let githubLoginUrl = getGithubLoginUrl _callbackUrl settings
            loggedIn = isJust user
        pure $
          [shamlet|
            <h3> Home - Example with Cookies
            <p>
                $if not loggedIn
                  <a href="#{githubLoginUrl}"> Login
                $else
                  Welcome #{fromJust user}!
          |]
    , auth = authServer
    }


main :: IO ()
main = do
  eitherConfig <- decodeFileExact configCodec ("./config.toml")
  config <-
    either
      (\errors -> fail $ "unable to parse configuration: " <> show errors)
      pure
      eitherConfig

  key <- getDefaultKey
  let ghSettings = mkSettings key (_githubOAuth config)
      context = optionalUserAuthHandler key :. oauth2AuthHandler ghSettings :. EmptyContext
      nat = id

  putStrLn "Waiting for connections!"
  run 8080 $
    genericServeTWithContext nat (server (_githubOAuth config) ghSettings) context
