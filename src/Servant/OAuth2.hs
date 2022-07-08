module Servant.OAuth2 where

import "base" Data.Typeable (Typeable)

data OAuth2Provider (tag :: k) deriving (Typeable)
