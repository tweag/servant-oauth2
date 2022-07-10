{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Config
import "base" Control.Monad.IO.Class (liftIO)
import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "warp" Network.Wai.Handler.Warp (run)
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2 (
  OAuth2 (..),
 )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Github (
  Github (..),
  mkGithubProvider,
 )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.Provider qualified as Wai
import "servant-server" Servant (
  AuthProtect,
  Context (EmptyContext, (:.)),
  Get,
  Handler,
  Header,
  Headers,
  NamedRoutes,
  NoContent (NoContent),
  StdMethod (GET),
  UVerb,
  Union,
  WithStatus (WithStatus),
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
import "clientsession" Web.ClientSession (Key, getDefaultKey)
import "cookie" Web.Cookie (SetCookie)


-- Servant OAuth2 Stuff ===================================

type OAuth2Result = '[WithStatus 303 RedirectWithCookie]


type instance AuthServerData (AuthProtect "oauth2-github") = Union OAuth2Result


data Routes mode = Routes
  { home :: mode :- Get '[HTML] Html
  , auth ::
      mode
        :- AuthProtect "oauth2-github"
          :> "auth"
          :> "github"
          :> NamedRoutes (OAuth2Routes OAuth2Result)
  }
  deriving stock (Generic)


-- mkSettings :: Key -> OAuthConfig -> OAuth2Settings Github OAuth2Result
-- mkSettings key c = settings
--  where
--   toSessionId = pure . id
--   provider = mkGithubProvider (_name c) (_id c) (_secret c) emailAllowList Nothing
--   settings = undefined -- simpleCookieOAuth2Settings provider toSessionId key
--   emailAllowList = [".*"]


-- server ::
--   OAuthConfig ->
--   OAuth2Settings Github OAuth2Result ->
--   Routes (AsServerT Handler)
-- server OAuthConfig {_callbackUrl} settings =
--   Routes
--     { home = do
--         let (Github {githubOAuth2}) = provider settings
--             githubLoginUrl = getRedirectUrl _callbackUrl githubOAuth2 (oa2Scope githubOAuth2)
--         liftIO $ print githubLoginUrl
--         pure $
--           [shamlet|
--             <h3> Home - Example with Cookies
--             <p>
--                 <a href="#{githubLoginUrl}"> Login
--           |]
--     , auth = authServer
--     }


main = undefined
-- main :: IO ()
-- main = do
--   eitherConfig <- decodeFileExact configCodec ("./example-cookies/config.toml")
--   config <-
--     either
--       (\errors -> fail $ "unable to parse configuration: " <> show errors)
--       pure
--       eitherConfig

--   key <- getDefaultKey
--   let ghSettings = mkSettings key (_oauth config)
--       context = oauth2AuthHandler ghSettings :. EmptyContext
--       nat = id

--   run 8080 $
--     genericServeTWithContext nat (server (_oauth config) ghSettings) context
