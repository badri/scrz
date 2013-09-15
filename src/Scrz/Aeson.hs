module Scrz.Aeson
  ( deriveScrzJSON
  ) where


import Language.Haskell.TH
import Data.Aeson.TH
import Data.Char


deriveScrzJSON :: String -> Name -> Q [ Dec ]
deriveScrzJSON prefix = deriveJSON options
  where
    options = defaultOptions
            { fieldLabelModifier = map toLower . drop (length prefix)
            }
