{-# language NamedFieldPuns  #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}

{-|

This example follows the "Servant.OAuth2.Examples.Simple" example very
closely, but this time we use a configuration that let's enables us to
set a cookie, and then redirect to the homepage.

Moreover, we set things up so that we can /read/ that cookie on /any/ page, to
determine if the current visitor is logged in.

We will assume you have read the "Simple" example, and mostly spend our time
explaining what is different.

-}

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


-- | This time our result type is a set of headers that both redirects, and
-- sets a particular cookie value. The cookie will, here, contain simply the
-- result of the oauth2 workflow; i.e. the users email.
--
-- @since 0.1.0.0
type OAuth2Result = '[WithStatus 303 RedirectWithCookie]


-- | Our instance here is exactly the same (in fact, it will _always_ be the
-- same!); it just connects the 'Github' type and the 'OAuth2Result' type, so
-- it can be picked out by the right version of 'oauth2AuthHandler'.
--
-- @since 0.1.0.0
type instance AuthServerData (AuthProtect Github) = Tag Github OAuth2Result


-- | Now, we want to be able to check if a user is logged in on any page. We
-- will use this 'AuthProtect' instance to do that.
--
-- The _result_ of this particular check could typically be some kind of
-- @User@ value, but here, we're not concerning ourselves with that detail, so
-- we will just return a 'Maybe Text'; i.e. either 'Nothing', if we couldn't
-- decode a user from the cookie, or the ident of the user if we could.
--
-- @since 0.1.0.0
type instance AuthServerData (AuthProtect "optional-cookie") = Maybe Text


-- | This is the corresponding handler for the above instance. Our
-- implementation is very simple, we just call 'getSessionIdFromCookie', which
-- is provided by the "Servant.OAuth2" library itself; this decodes a
-- previously-encoded value from the cookie, by the corresponding function
-- 'buildSessionCookie', which we will later use through the
-- 'simpleCookieOAuth2Settings' function.
--
-- @since 0.1.0.0
optionalUserAuthHandler :: Key -> AuthHandler Request (Maybe Text)
optionalUserAuthHandler key = mkAuthHandler f
 where
  f :: Request -> Handler (Maybe Text)
  f req = do
    let sessionId = getSessionIdFromCookie req key
    pure sessionId


-- | As last time, we have our routes; the main change is the inclusion of the
-- 'AuthProtect' tag on the 'home' route, that let's us bring a potential user
-- into scope for that page.
--
-- @since 0.1.0.0
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


-- | Again, we have settings, but this time, instead of using the
-- 'defaultOAuth2Settings', we use the 'simpleCookieOAuth2Settings' function
-- to get default behaviour that, upon successful completion of the oauth2
-- flow, builds a cookie with a /session id/ &mdash; in this case just the
-- ident of the user &mdash; and then redirects the browser to the homepage.
--
-- @since 0.1.0.0
mkSettings :: Key -> OAuthConfig -> OAuth2Settings Handler Github OAuth2Result
mkSettings key c = settings
 where
  toSessionId _ = pure . id
  provider = mkGithubProvider (_name c) (_id c) (_secret c) emailAllowList Nothing
  settings = simpleCookieOAuth2Settings provider toSessionId key
  emailAllowList = [".*"]


-- | Now we can have a simple server implementation, but this time we can
-- check if the user us logged in by looking at the first parameter to the
-- 'home' function; i.e. if it's 'Nothing' then we're not logged in, otherwise
-- we are! Very convenient.
--
-- @since 0.1.0.0
server :: OAuthConfig
       -> OAuth2Settings Handler Github OAuth2Result
       -> Routes (AsServerT Handler)
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


-- | Our entrypoint; the only addition here is that we need to obtain a 'Key'
-- to do our cookie encryption/decryption; and we again need to build up our
-- context with our 'Github'-based 'oauth2AuthHandler' and our own custom one,
-- 'optionalUserAuthHandler', to decode the cookie.
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

  key <- getDefaultKey

  let ghSettings = mkSettings key (_githubOAuth config)
      context =  optionalUserAuthHandler key
              :. oauth2AuthHandler ghSettings nat
              :. EmptyContext
      nat = id

  putStrLn "Waiting for connections!"
  run 8080 $
    genericServeTWithContext nat (server (_githubOAuth config) ghSettings) context
