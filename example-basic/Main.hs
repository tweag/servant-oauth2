{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Config
import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "warp" Network.Wai.Handler.Warp (run)
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Github
  ( Github (..)
  , mkGithubProvider
  )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Google
  ( Google (..)
  , mkGoogleProvider
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
import Servant.OAuth2.Hacks
import "servant-server" Servant.Server.Experimental.Auth
  ( AuthServerData
  )
import "servant-server" Servant.Server.Generic
  ( AsServerT
  , genericServeTWithContext
  )
import "shakespeare" Text.Hamlet (Html, shamlet)
import "tomland" Toml (decodeFileExact)


-- We need to define an instance that corresponds to the result we want to
-- return. We're going with the 'basic' option; so we'll just take the Text
-- value of the ident that comes back.
type OAuth2Result = '[WithStatus 200 Text]


-- This is the instance that connects up the route with the auth handler by
-- way of the return type.
type instance AuthServerData (AuthProtect "oauth2-github") = Tag Github OAuth2Result
type instance AuthServerData (AuthProtect "oauth2-google") = Tag Google OAuth2Result


data Routes mode = Routes
  { home :: mode :- Get '[HTML] Html
  , authGithub ::
      mode
        :- AuthProtect "oauth2-github"
          :> "auth"
          :> "github"
          :> NamedRoutes (OAuth2Routes OAuth2Result)
  , authGoogle ::
      mode
        :- AuthProtect "oauth2-google"
          :> "auth"
          :> "google"
          :> NamedRoutes (OAuth2Routes OAuth2Result)
  }
  deriving stock (Generic)


-- The final connecttion: the settings we pass in need to specify the return
-- result that should come back.
mkGithubSettings :: OAuthConfig -> OAuth2Settings Github OAuth2Result
mkGithubSettings c =
  defaultOAuth2Settings $
    mkGithubProvider (_name c) (_id c) (_secret c) emailAllowList Nothing
 where
  emailAllowList = [".*"]


-- The final connecttion: the settings we pass in need to specify the return
-- result that should come back.
mkGoogleSettings :: OAuthConfig -> OAuth2Settings Google OAuth2Result
mkGoogleSettings c =
  defaultOAuth2Settings $
    mkGoogleProvider (_id c) (_secret c) emailAllowList Nothing
 where
  emailAllowList = [".*"]


server ::
  Text ->
  OAuth2Settings Github OAuth2Result ->
  Text ->
  OAuth2Settings Google OAuth2Result ->
  Routes (AsServerT Handler)
server githubCallbackUrl githubSettings googleCallbackUrl googleSettings =
  Routes
    { home = do
        -- let (Github {githubOAuth2}) = provider githubSettings
        --     githubLoginUrl = getRedirectUrl githubCallbackUrl githubOAuth2 (oa2Scope githubOAuth2)
        -- let (Google {googleOAuth2}) = provider googleSettings
        --     googleLoginUrl = getRedirectUrl googleCallbackUrl googleOAuth2 (oa2Scope googleOAuth2)

        let githubLoginUrl = getGithubLoginUrl githubCallbackUrl githubSettings
            googleLoginUrl = getGoogleLoginUrl googleCallbackUrl googleSettings

        pure $
          [shamlet|
            <h3> Home - Basic Example
            <p>
                <a href="#{githubLoginUrl}"> Github Login
                <br>
                <a href="#{googleLoginUrl}"> Google Login
          |]
    , authGithub = authServer
    , authGoogle = authServer
    }


main :: IO ()
main = do
  eitherConfig <- decodeFileExact configCodec ("./config.toml")
  config <-
    either
      (\errors -> fail $ "unable to parse configuration: " <> show errors)
      pure
      eitherConfig

  let githubSettings = mkGithubSettings (_githubOAuth config)
      googleSettings = mkGoogleSettings (_googleOAuth config)
      context =  oauth2AuthHandler githubSettings
              :. oauth2AuthHandler googleSettings
              :. EmptyContext
      nat = id

  run 8080 $
    genericServeTWithContext nat (server (_callbackUrl (_githubOAuth config)) githubSettings (_callbackUrl (_googleOAuth config)) googleSettings) context
