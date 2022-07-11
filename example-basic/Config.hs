module Config where

import "text" Data.Text (Text)
import "tomland" Toml (TomlCodec, diwrap, table, text, (.=))


data OAuthConfig = OAuthConfig
  { _name :: Text
  , _id :: Text
  , _secret :: Text
  , _callbackUrl :: Text
  }


oauthConfigCodec :: TomlCodec OAuthConfig
oauthConfigCodec =
  OAuthConfig
    <$> diwrap (text "name") .= _name
    <*> diwrap (text "id") .= _id
    <*> diwrap (text "secret") .= _secret
    <*> diwrap (text "callback_url") .= _callbackUrl


data Config = Config
  { _githubOAuth :: OAuthConfig
  , _googleOAuth :: OAuthConfig
  }


configCodec :: TomlCodec Config
configCodec =
  Config
    <$> table oauthConfigCodec "oauth-github" .= _githubOAuth
    <*> table oauthConfigCodec "oauth-google" .= _googleOAuth
