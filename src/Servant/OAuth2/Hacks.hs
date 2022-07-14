{-|
A collection of hacky functions needed to work around the fact that
'wai-middleware-auth' is presently defined in terms of Wai, and, really, we
want something that is defined for Servant.
-}
{-# language NamedFieldPuns #-}
{-# language ViewPatterns   #-}
{-# language TupleSections  #-}

module Servant.OAuth2.Hacks where

import "text" Data.Text (Text, intercalate)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import "hoauth2" Network.OAuth.OAuth2 qualified as OA2
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2
  ( OAuth2 (..)
  )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2 qualified as Wai
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Github
  ( Github (..)
  )
import "wai-middleware-auth" Network.Wai.Middleware.Auth.OAuth2.Google
  ( Google (..)
  )
import Servant.OAuth2
import "uri-bytestring" URI.ByteString qualified as U


-- | For some settings specialised to 'Github', return the login url.
--
-- @since 0.1.0.0
getGithubLoginUrl :: Text -> OAuth2Settings Github a  -> Text
getGithubLoginUrl callbackUrl (provider -> Github { githubOAuth2 })
  = getRedirectUrl callbackUrl githubOAuth2 (oa2Scope githubOAuth2)


-- | For some settings specialised to 'Google', return the login url.
--
-- @since 0.1.0.0
getGoogleLoginUrl :: Text -> OAuth2Settings Google a  -> Text
getGoogleLoginUrl callbackUrl (provider -> Google { googleOAuth2 })
  = getRedirectUrl callbackUrl googleOAuth2 (oa2Scope googleOAuth2)


-- | An extremely unfortunate way of getting the redirect URL; stolen from
-- 'Network.Wai.Auth.Internal'.
--
-- @since 0.1.0.0
getRedirectUrl :: Text -> Wai.OAuth2 -> Maybe [Text] -> Text
getRedirectUrl callbackUrl waiOa2 oa2Scope = decodeUtf8 redirectUrl
 where
  scope = (encodeUtf8 . intercalate " ") <$> oa2Scope
  redirectUrl =
    getRedirectURI $
      (flip OA2.appendQueryParams)
        (OA2.authorizationUrl oa2)
        (maybe [] ((: []) . ("scope",)) scope)
  oa2 = maybe (error "Couldn't construct the OAuth2 record.") id fromInteralOAuth2
  getRedirectURI = U.serializeURIRef'
  parseAbsoluteURI urlTxt = do
    case U.parseURI U.strictURIParserOptions (encodeUtf8 urlTxt) of
      Left _ -> Nothing
      Right url -> pure url
  fromInteralOAuth2 = do
    authEndpointURI <- parseAbsoluteURI $ Wai.oa2AuthorizeEndpoint waiOa2
    callbackURI <- parseAbsoluteURI callbackUrl
    pure $
      OA2.OAuth2
        { OA2.oauth2ClientId = Wai.oa2ClientId waiOa2
        , OA2.oauth2ClientSecret = error "Client secret not needed."
        , OA2.oauth2AuthorizeEndpoint = authEndpointURI
        , OA2.oauth2TokenEndpoint = error "No token endpoint"
        , OA2.oauth2RedirectUri = callbackURI
        }
