{-# language NamedFieldPuns  #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}

module Servant.OAuth2.Examples.Authorisation where

import Servant.OAuth2.Examples.Config
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
import "cookie" Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)


type Db = H.HashMap Text User


data Role = Anyone | Admin


data User = User
  { email :: Text
  , role  :: Text
  }
  deriving stock (Show)


data Env (r :: Role) = Env
  { user              :: Maybe User
  , githubSettings    :: OAuth2Settings Github OAuth2Result
  , githubOAuthConfig :: OAuthConfig
  , googleSettings    :: OAuth2Settings Google OAuth2Result
  , googleOAuthConfig :: OAuthConfig
  }


type PageM      = ReaderT (Env 'Anyone) Handler
type AdminPageM = ReaderT (Env 'Admin)  Handler


type OAuth2Result = '[WithStatus 303 RedirectWithCookie]


type instance AuthServerData (AuthProtect "oauth2-github") = Tag Github OAuth2Result


type instance AuthServerData (AuthProtect "oauth2-google") = Tag Google OAuth2Result


type instance AuthServerData (AuthProtect "optional-cookie") = Maybe User


optionalUserAuthHandler :: Db -> Key -> AuthHandler Request (Maybe User)
optionalUserAuthHandler db key = mkAuthHandler f
 where
  f :: Request -> Handler (Maybe User)
  f req = do
    let sessionId = getSessionIdFromCookie req key
    -- Here, we know the sessionId is, infact, the email address of the user.
    -- So, we can just look it up in the database.
    pure $ maybe Nothing (flip H.lookup db . decodeUtf8) sessionId


data Routes mode = Routes
  { site :: mode :- AuthProtect "optional-cookie" :> NamedRoutes (SiteRoutes)
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


data SiteRoutes mode = SiteRoutes
  { home   :: mode :- Get '[HTML] Html
  , admin  :: mode :- "admin" :> NamedRoutes AdminRoutes
  , logout :: mode :- "logout" :> UVerb 'GET '[HTML] '[WithStatus 303 RedirectWithCookie]
  }
  deriving stock (Generic)


emptyCookie :: SetCookie
emptyCookie = defaultSetCookie
  { setCookieName     = ourCookie
  , setCookieValue    = ""
  , setCookieMaxAge   = Just 0
  , setCookiePath     = Just "/"
  , setCookieSameSite = Just sameSiteStrict
  , setCookieHttpOnly = True
  , setCookieSecure   = False
  }


siteServer :: SiteRoutes (AsServerT PageM)
siteServer = SiteRoutes
  { home   = homeHandler
  , admin  = adminServer
  , logout = respond $ WithStatus @303 (redirectWithCookie "/" emptyCookie)
  }


data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)



adminHandler :: AdminPageM Html
adminHandler = do
  let secrets =
        [ "secret 1" :: Text
        , "mundane secret 2"
        , "you can't know this"
        ]
  u <- getAdmin
  pure $
    [shamlet|
      <h3> Admin
      <p> Secrets:

      <ul>
        $forall secret <- secrets
          <li> #{secret}

      <p> Hello Admin person whose identity is: #{show u}.
    |]


adminServer :: ServerT (NamedRoutes AdminRoutes) PageM
adminServer = verifyAdmin $ AdminRoutes
  { adminHome = adminHandler
  }


isAdmin :: Maybe User -> Bool
isAdmin (Just (User {role})) = role == "admin"
isAdmin _ = False


isLoggedIn :: PageM Bool
isLoggedIn = pure . isJust .user =<< ask


getUser :: PageM (Maybe User)
getUser = pure . user =<< ask


getAdmin :: AdminPageM (User)
getAdmin = pure . fromJust . user =<< ask


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


homeHandler :: PageM Html
homeHandler = do
  env <- ask

  let githubCallbackUrl = _callbackUrl $ githubOAuthConfig env
      githubLoginUrl = getGithubLoginUrl githubCallbackUrl (githubSettings env)

  let googleCallbackUrl = _callbackUrl $ googleOAuthConfig env
      googleLoginUrl = getGoogleLoginUrl googleCallbackUrl (googleSettings env)

  loggedIn <- isLoggedIn
  u <- getUser
  pure $
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


mkGithubSettings :: Key -> OAuthConfig -> OAuth2Settings Github OAuth2Result
mkGithubSettings key c = settings
 where
  toSessionId = pure . id
  provider = mkGithubProvider (_name c) (_id c) (_secret c) emailAllowList Nothing
  settings = simpleCookieOAuth2Settings provider toSessionId key
  emailAllowList = [".*"]


mkGoogleSettings :: Key -> OAuthConfig -> OAuth2Settings Google OAuth2Result
mkGoogleSettings key c = settings
 where
  toSessionId = pure . id
  provider = mkGoogleProvider (_id c) (_secret c) emailAllowList Nothing
  settings = simpleCookieOAuth2Settings provider toSessionId key
  emailAllowList = [".*"]


main :: IO ()
main = do
  eitherConfig <- decodeFileExact configCodec ("./config.toml")
  config <-
    either
      (\errors -> fail $ "unable to parse configuration: " <> show errors)
      pure
      eitherConfig

  key <- getDefaultKey
  db <- loadDb

  let githubSettings = mkGithubSettings key (_githubOAuth config)
      googleSettings = mkGoogleSettings key (_googleOAuth config)
      env = Env Nothing
              githubSettings (_githubOAuth config)
              googleSettings (_googleOAuth config)
      context
        =  optionalUserAuthHandler db key
        :. oauth2AuthHandler githubSettings
        :. oauth2AuthHandler googleSettings
        :. EmptyContext

  let nat :: PageM a -> Handler a
      nat = flip runReaderT env

  putStrLn "Waiting for connections!"
  run 8080 $
    genericServeTWithContext nat server context


loadDb :: IO Db
loadDb = do
  ls <- Text.lines <$> Text.readFile "./servant-oauth2-examples/example-auth/db.txt"
  let raw = map (Text.split (==',')) ls
      mkRow [u,r] = (u, User u r)
      mkRow _ = error "Inconsistent database state."
  pure $ H.fromList $ map mkRow raw

