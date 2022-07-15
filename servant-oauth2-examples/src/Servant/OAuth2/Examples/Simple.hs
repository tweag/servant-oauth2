{-# language NamedFieldPuns  #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}
{-|

This is the simplest example of a full application that makes use of this
library.

We don't do anything with the result of successful authentication other than
return the ident that was provided to us. In an "real" example, you'll want to
set a cookie. For that, you can take a look at
"Servant.OAuth2.Examples.Cookies".

This file serves as a complete example; and you can read through this
documentation from top to bottom, in order to work out what each component is.
-}

module Servant.OAuth2.Examples.Simple where

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
import Servant.OAuth2.Examples.Config
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


-- | First, we need to define an instance that corresponds to the result we
-- want to return. We're going with the 'basic' option; so we'll just take the
-- Text value of the ident that comes back. Note that this is a _list_ of
-- potential return kinds; the reason it's set up this way is only so we can
-- explicitly say we'd like to return a 303 Redirect, when using cookies.
--
-- @since 0.1.0.0
type OAuth2Result = '[WithStatus 200 Text]


-- | Next up, we follow the approach of the generalised servant authentication
-- to connect up our (future usage of) the 'oauth2AuthHandler' to the
-- respective tagged routes by by this particular 'AuthProtect' instance,
-- namely, the 'authGithub' and 'authGoogle' routes we will define in a
-- moment.
--
-- @since 0.1.0.0
type instance AuthServerData (AuthProtect Github) = Tag Github OAuth2Result


-- | Same as above, but for google.
--
-- @since 0.1.0.0
type instance AuthServerData (AuthProtect Google) = Tag Google OAuth2Result


-- | Here we just define a very simple website, something like:
--
-- @
--  \/
--  \/auth\/github\/...
--  \/auth\/google\/...
-- @
--
-- The 'authGoogle' and 'authGithub' routes will not be implemented by us;
-- they are both provided by a 'NamedRoutes (OAuth2Routes OAuth2Result)'
-- value; i.e. the routes themselves come from 'Servant.OAuth2'.
--
-- @since 0.1.0.0
data Routes mode = Routes
  { home :: mode :- Get '[HTML] Html
  , authGithub ::
      mode
        :- AuthProtect Github
          :> "auth"
          :> "github"
          :> NamedRoutes (OAuth2Routes OAuth2Result)
  , authGoogle ::
      mode
        :- AuthProtect Google
          :> "auth"
          :> "google"
          :> NamedRoutes (OAuth2Routes OAuth2Result)
  }
  deriving stock (Generic)


-- | We need to build an 'OAuth2Settings' to pass to 'oauth2AuthHandler', so
-- that it knows which provider it is working with. We also need to tag it
-- with a 'Handler'-like monad that can interpret errors; in the simple case
-- this is just the 'Handler' type itself, but in later examples (in
-- particular the "Servant.OAuth2.Examples.Authorisation" example) it will be
-- a custom monad.
--
-- @since 0.1.0.0
mkGithubSettings :: OAuthConfig -> OAuth2Settings Handler Github OAuth2Result
mkGithubSettings c =
  defaultOAuth2Settings $
    mkGithubProvider (_name c) (_id c) (_secret c) emailAllowList Nothing
 where
  emailAllowList = [".*"]


-- | Exactly the same as 'mkGithubSettings' but for the 'Google' provider.
--
-- @since 0.1.0.0
mkGoogleSettings :: OAuthConfig -> OAuth2Settings Handler Google OAuth2Result
mkGoogleSettings c =
  defaultOAuth2Settings $
    mkGoogleProvider (_id c) (_secret c) emailAllowList Nothing
 where
  emailAllowList = [".*"]


-- | Here we pull implement a very simple homepage, basically just showing the
-- links to login, and connecting the two 'authGithub' and 'authGoogle' routes
-- together. There's a bit of noise in passing all the relevant configs in,
-- but this would go away in a "real" application, by passing that around in
-- an env, or otherwise.
--
-- @since 0.1.0.0
server ::
  Text ->
  OAuth2Settings Handler Github OAuth2Result ->
  Text ->
  OAuth2Settings Handler Google OAuth2Result ->
  Routes (AsServerT Handler)
server githubCallbackUrl githubSettings googleCallbackUrl googleSettings =
  Routes
    { home = do
        let githubLoginUrl = getGithubLoginUrl githubCallbackUrl githubSettings
            googleLoginUrl = getGoogleLoginUrl googleCallbackUrl googleSettings

        pure $
          [shamlet|
            <h3> Home - Basic Example
            <p>
              <a href="#{githubLoginUrl}"> Github Login
            <p>
              <a href="#{googleLoginUrl}"> Google Login
          |]
    , authGithub = authServer
    , authGoogle = authServer
    }


-- | Entrypoint. The most important thing we do here is build our list of
-- contexts by calling 'oauth2AuthHandler' with the respective settings.
--
-- @since 0.1.0.0
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
      context =  oauth2AuthHandler githubSettings nat
              :. oauth2AuthHandler googleSettings nat
              :. EmptyContext
      nat = id
      server' = server (_callbackUrl (_githubOAuth config))
                       githubSettings
                       (_callbackUrl (_googleOAuth config))
                       googleSettings

  putStrLn "Waiting for connections!"
  run 8080 $ genericServeTWithContext nat server' context
