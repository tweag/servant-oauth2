{-# language NamedFieldPuns  #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}

{-|

This is the last example we provide, but also the most interesting, and,
indeed, the main motivation for this libraries existence!

Here we show how to build type-level authorisation into your Servant API,
backed by authentication with OAuth2.

We assume you've read over the previous two examples, as we build directly
on that knowledge:

- "Servant.OAuth2.Examples.Simple"
- "Servant.OAuth2.Examples.Cookies"

-}

module Servant.OAuth2.Examples.Authorisation where

import "mtl" Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import "base" Data.Coerce (coerce)
import "unordered-containers" Data.HashMap.Strict qualified as H
import "base" Data.Maybe (fromJust, isJust)
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "text" Data.Text.Encoding (decodeUtf8)
import "text" Data.Text.IO qualified as Text
import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Request)
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
  , Proxy (Proxy)
  , ServerT
  , StdMethod (GET)
  , UVerb
  , WithStatus (WithStatus)
  , err404
  , hoistServer
  , respond
  , throwError
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


-- | This time we're going to have users. We're keeping it light and easy
-- here, so our /database/ is simply a map of emails to users. At this point
-- I'd like to note a slight quirk of oauth2-based authentication.
--
-- Note that the ident that comes back from the provider is up to that
-- provider itself. So, for example, I could make an entirely new oauth2
-- provider that always returns the same email, for example. In particular, it
-- could always return _you_ email. Then, if this website added my (dodgey)
-- provider to it's list, I would be able to log in as you, if all you to do
-- verify accounts is /look up the user by the email/. So, in any real system,
-- you should track the /provider name/ along side the user ident, and only
-- use /that/ combination to find users. We don't do that here, but it's worth
-- remembering.
--
-- @since 0.1.0.0
type Db = H.HashMap Text User


-- | We will use this type to tag particular routes as being only accessible
-- to users with the 'Admin' role, or, alternatively, /everyone/, i.e. those
-- people having the 'Anyone' role ... namely, everyone!
--
-- @since 0.1.0.0
data Role = Anyone | Admin


-- | Our user type that lives in the database. Importantly, this holds the
-- 'role', which we will check when it comes to verifying if a particular
-- person can access the 'Admin' route.
--
-- @since 0.1.0.0
data User = User
  { email :: Text
  , role  :: Text
  }
  deriving stock (Show)


-- | This is a collection of data that we'll want to have available during
-- page processing; so we will wrap the servant 'Handler' type with a
-- 'ReaderT' over this type.
--
-- @since 0.1.0.0
data Env (r :: Role) = Env
  { user              :: Maybe User
  , githubSettings    :: OAuth2Settings PageM Github OAuth2Result
  , githubOAuthConfig :: OAuthConfig
  , googleSettings    :: OAuth2Settings PageM Google OAuth2Result
  , googleOAuthConfig :: OAuthConfig
  }


-- | Our type-level authorisation system. We tag two kinds of /page monads/;
-- one that works for 'Anyone'; this one.
--
-- @since 0.1.0.0
type PageM      = ReaderT (Env 'Anyone) Handler


-- | And this one, that is specialised to 'Admin' users. If we make a mistake,
-- we will get a type error along the lines of @Cannot match 'Admin with
-- 'Anyone@.
--
-- @since 0.1.0.0
type AdminPageM = ReaderT (Env 'Admin)  Handler


-- | As in the "Servant.OAuth2.Examples.Cookies" example, our result type is
-- just a redirection with a cookie.
--
-- @since 0.1.0.0
type OAuth2Result = '[WithStatus 303 RedirectWithCookie]


-- | Again, we exactly follow the "Servant.OAuth2.Examples.Cookies" example.
--
-- @since 0.1.0.0
type instance AuthServerData (AuthProtect Github) = Tag Github OAuth2Result


-- | Same here.
--
-- @since 0.1.0.0
type instance AuthServerData (AuthProtect Google) = Tag Google OAuth2Result


-- | The only difference here is the return a 'User' instead of 'Text'.
--
-- @since 0.1.0.0
type instance AuthServerData (AuthProtect "optional-cookie") = Maybe User


-- | This is almost identical to the "Servant.OAuth2.Examples.Cookies"
-- example, except we look up the user in the database, and if we find it, we
-- return it.
--
-- @since 0.1.0.0
optionalUserAuthHandler :: Db -> Key -> AuthHandler Request (Maybe User)
optionalUserAuthHandler db key = mkAuthHandler f
 where
  f :: Request -> Handler (Maybe User)
  f req = do
    let sessionId = getSessionIdFromCookie req key
    -- Here, we know the sessionId is, infact, the email address of the user.
    -- So, we can just look it up in the database.
    pure $ maybe Nothing (flip H.lookup db . decodeUtf8) sessionId


-- | This follows exactly the "Servant.OAuth2.Examples.Cookies" example; we're
-- using two providers because in the hard-coded `db.txt` file I've set
-- different roles for my own account with different providers; you'll be able
-- to edit that file to do the same.
--
-- @since 0.1.0.0
data Routes mode = Routes
  { site :: mode :- AuthProtect "optional-cookie" :> NamedRoutes SiteRoutes
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


-- | We now have a slightly more complicated route setup; we need our
-- homepage, and our admin area, which we will aim to protect with our
-- type-level tags; we also need a 'logout' route, because it'll be convenient
-- for testing. This route will simply delete the present cookie.
--
-- @since 0.1.0.0
data SiteRoutes mode = SiteRoutes
  { home   :: mode :- Get '[HTML] Html
  , admin  :: mode :- "admin" :> NamedRoutes AdminRoutes
  , logout :: mode :- "logout" :> UVerb 'GET '[HTML] '[WithStatus 303 RedirectWithCookie]
  }
  deriving stock (Generic)


-- | Nothing too innovative; we just pass off to respective handlers and
-- servers; in the 'logout' route we set an empty cookie and redirect home.
--
-- @since 0.1.0.0
siteServer :: SiteRoutes (AsServerT PageM)
siteServer = SiteRoutes
  { home   = homeHandler
  , admin  = adminServer
  , logout = respond $ WithStatus @303 (redirectWithCookie "/" emptyCookie)
  }


-- | Our admin routes. At this point they look normal.
--
-- @since 0.1.0.0
data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)


-- | Here is where we introduce the 'AdminPageM' type. Typically, a handler
-- like this would have type 'Handler'; but here we're denoting it as having
-- the 'AdminPageM' type. This means we can call specific functions, that we
-- will define below, such as 'getAdmin'. Importantly, we will see that we
-- need to unwrap this type (by verifying the current user!) before we can
-- render this page.
--
-- @since 0.1.0.0
adminHandler :: AdminPageM Html
adminHandler = do
  let secrets =
        [ "secret 1" :: Text
        , "mundane secret 2"
        , "you can't know this"
        ]
  u <- getAdmin
  pure
    [shamlet|
      <h3> Admin
      <p> Secrets:

      <ul>
        $forall secret <- secrets
          <li> #{secret}

      <p> Hello Admin person whose identity is: #{show u}.
    |]


-- | Here's the most important function. We aim to convert 'AdminPageM's into
-- 'PageM's. We do this in the context of an 'PageM' function, where we
-- investigate the current user. If that user is an admin (vi a'isAdmin') then
-- we convert the given 'AdminPageM' into a 'PageM' by simply 'coerce'ing it;
-- after all, the 'Role' type was just a phantom type.
--
-- If we fail to verify that they are an admin, we throw a http 404 error.
--
-- @since 0.1.0.0
verifyAdmin :: ServerT (NamedRoutes AdminRoutes) AdminPageM
            -> ServerT (NamedRoutes AdminRoutes) PageM
verifyAdmin = hoistServer (Proxy @(NamedRoutes AdminRoutes)) transform
  where
    transform :: AdminPageM a -> PageM a
    transform p = do
      env <- ask
      let currentUser = user env
      if isAdmin currentUser
         then coerce p
         else throwError err404


-- | Note here that this function returns a server of 'PageM's; that's because
-- we pass the routes through the 'verifyAdmin' function.
--
-- @since 0.1.0.0
adminServer :: ServerT (NamedRoutes AdminRoutes) PageM
adminServer = verifyAdmin $ AdminRoutes
  { adminHome = adminHandler
  }


-- | A simple check to see if the user is present and has a 'role' that is
-- equal to `"admin"`.
--
-- @since 0.1.0.0
isAdmin :: Maybe User -> Bool
isAdmin (Just User {role}) = role == "admin"
isAdmin _ = False


-- | Check if a user is present and therefore logged in.
--
-- @since 0.1.0.0
isLoggedIn :: PageM Bool
isLoggedIn = isJust <$> getUser


-- | In the context of a 'PageM', maybe return the user; this is the best we
-- can do.
--
-- @since 0.1.0.0
getUser :: PageM (Maybe User)
getUser = user <$> ask


-- | In the present of an 'AdminPageM', /definitely/ return a user. We're
-- happy with an error if this fails, because we know that a user needs to be
-- present.
--
-- Note that it could be an extension to this code to eliminate the 'fromJust'
-- here, and ensure that whatever context we're referencing has eliminated the
-- 'Maybe' over the user.
--
-- We leave this as an exercise for the reader :)
--
-- @since 0.1.0.0
getAdmin :: AdminPageM User
getAdmin = fromJust . user <$> ask


-- | This time our home handler does a bit of busywork to show whether or not
-- you're logged in, and provide the relevant links. It also detects if you're
-- an admin, and if not, provides you a link to the admin page anyway, to see
-- if you can hack into it! :)
--
-- @since 0.1.0.0
homeHandler :: PageM Html
homeHandler = do
  env <- ask

  let githubCallbackUrl = _callbackUrl $ githubOAuthConfig env
      githubLoginUrl = getGithubLoginUrl githubCallbackUrl (githubSettings env)

  let googleCallbackUrl = _callbackUrl $ googleOAuthConfig env
      googleLoginUrl = getGoogleLoginUrl googleCallbackUrl (googleSettings env)

  loggedIn <- isLoggedIn
  u <- getUser
  pure
    [shamlet|
      <h3> Home - Example with authorisation

      $if not loggedIn
        <p>
          <a href="#{githubLoginUrl}"> Login with Github
          <br>
          <a href="#{googleLoginUrl}"> Login with Google
      $else
        Welcome #{show u}!
        <p>
          <a href="/logout"> Logout

      $if isAdmin u
        <p>
          <a href="/admin"> Access the admin area!
      $else
        <p>
          You're not an admin, but perhaps you may like to
          <a href="/admin"> try and hack into the admin area!
    |]


-- | The final full server; we need a special 'hoistServer' for the 'site'
-- route, because we need to add the 'Maybe User' into the 'Env'. Otherwise,
-- we just do as we've always done - pass off to the 'authServer'.
--
-- @since 0.1.0.0
server :: Routes (AsServerT PageM)
server =
  Routes
    { site = \user ->
        let addUser env = env { user = user }
        in hoistServer
            (Proxy @(NamedRoutes SiteRoutes))
            (withReaderT addUser)
            siteServer
    , authGithub = authServer
    , authGoogle = authServer
    }


-- | Our usual approach for 'Github' settings.
--
-- @since 0.1.0.0
mkGithubSettings :: Key -> OAuthConfig -> OAuth2Settings PageM Github OAuth2Result
mkGithubSettings key c = settings
 where
  toSessionId _ = pure
  provider = mkGithubProvider (_name c) (_id c) (_secret c) emailAllowList Nothing
  settings = simpleCookieOAuth2Settings provider toSessionId key
  emailAllowList = [".*"]


-- | Our usual approach for 'Google' settings.
--
-- @since 0.1.0.0
mkGoogleSettings :: Key -> OAuthConfig -> OAuth2Settings PageM Google OAuth2Result
mkGoogleSettings key c = settings
 where
  toSessionId _ = pure
  provider = mkGoogleProvider (_id c) (_secret c) emailAllowList Nothing
  settings = simpleCookieOAuth2Settings provider toSessionId key
  emailAllowList = [".*"]


-- | Our usual approach to the 'main' function; setting up the settings,
-- setting up the contexts for the relevant auth handler functions.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  eitherConfig <- decodeFileExact configCodec "./config.toml"
  config <-
    either
      (\errors -> fail $ "unable to parse configuration: " <> show errors)
      pure
      eitherConfig

  key <- getDefaultKey
  db <- loadDb

  let nat :: PageM a -> Handler a
      nat = flip runReaderT env
      githubSettings = mkGithubSettings key (_githubOAuth config)
      googleSettings = mkGoogleSettings key (_googleOAuth config)
      env = Env Nothing
              githubSettings (_githubOAuth config)
              googleSettings (_googleOAuth config)
      context
        =  optionalUserAuthHandler db key
        :. oauth2AuthHandler githubSettings nat
        :. oauth2AuthHandler googleSettings nat
        :. EmptyContext


  putStrLn "Waiting for connections!"
  run 8080 $
    genericServeTWithContext nat server context


-- | Utility function to load the hard-coded database.
--
-- @since 0.1.0.0
loadDb :: IO Db
loadDb = do
  ls <- Text.lines <$> Text.readFile "./servant-oauth2-examples/example-auth/db.txt"
  let raw = map (Text.split (==',')) ls
      mkRow [u,r] = (u, User u r)
      mkRow _ = error "Inconsistent database state."
  pure $ H.fromList $ map mkRow raw
