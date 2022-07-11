{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Config
import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "warp" Network.Wai.Handler.Warp (run)
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2 (
  OAuth2 (..),
 )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Google (
  Google (..),
  mkGoogleProvider,
 )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Github (
  Github (..),
  mkGithubProvider,
 )
import "servant-server" Servant (
  AuthProtect,
  Context (EmptyContext, (:.)),
  Get,
  Handler,
  NamedRoutes,
  Union,
  WithStatus,
  type (:>),
 )
import "servant" Servant.API.Generic ((:-))
import "servant-blaze" Servant.HTML.Blaze (HTML)
import Servant.OAuth2
import "servant-server" Servant.Server.Experimental.Auth (
  AuthServerData,
 )
import "servant-server" Servant.Server.Generic (
  AsServerT,
  genericServeTWithContext,
 )
import "shakespeare" Text.Hamlet (Html, shamlet)
import "tomland" Toml (decodeFileExact)


-- We need to define an instance that corresponds to the result we want to
-- return. We're going with the 'basic' option; so we'll just take the Text
-- value of the ident that comes back.
type OAuth2Result = '[WithStatus 200 Text]


-- This is the instance that connects up the route with the auth handler by
-- way of the return type.
type instance AuthServerData (AuthProtect "oauth2-google") = T Google (Union OAuth2Result)
type instance AuthServerData (AuthProtect "oauth2-github") = T Github (Union OAuth2Result)

data T a b = T { unT :: b }

data Routes mode = Routes
  { home :: mode :- Get '[HTML] Html
  , auth1 :: mode :- AuthProtect "oauth2-google" :> "auth" :> "google" :> NamedRoutes (OAuth2Routes OAuth2Result)
  , auth2 :: mode :- AuthProtect "oauth2-github" :> "auth" :> "github" :> NamedRoutes (OAuth2Routes OAuth2Result)
  }
  deriving stock (Generic)


-- The final connecttion: the settings we pass in need to specify the return
-- result that should come back.
mkSettings :: OAuthConfig -> OAuth2Settings Google OAuth2Result
mkSettings c =
  defaultOAuth2Settings $
    mkGoogleProvider (_id c) (_secret c) emailAllowList Nothing
 where
  emailAllowList = [".*"]


server ::
  OAuthConfig ->
  OAuth2Settings Google OAuth2Result ->
  Routes (AsServerT Handler)
server OAuthConfig {_callbackUrl} settings =
  Routes
    { home = do
        let (Google {googleOAuth2}) = provider settings
            loginUrl = getRedirectUrl _callbackUrl googleOAuth2 (oa2Scope googleOAuth2)
        pure $
          [shamlet|
            <h3> Home - Basic Example
            <p>
                <a href="#{loginUrl}"> Login
          |]
    , auth = authServer
    }


main :: IO ()
main = do
  eitherConfig <- decodeFileExact configCodec ("./example-basic/config.toml")
  config <-
    either
      (\errors -> fail $ "unable to parse configuration: " <> show errors)
      pure
      eitherConfig

  let ghSettings = mkSettings (_oauth config)
      context = oauth2AuthHandler ghSettings :. EmptyContext
      nat = id

  run 8080 $
    genericServeTWithContext nat (server (_oauth config) ghSettings) context
