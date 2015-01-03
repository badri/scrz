{-# LANGUAGE TemplateHaskell #-}

module Scrz.TH
    ( deriveMyJSON
    ) where


import           Data.Aeson
import           Data.Aeson.TH
import           Language.Haskell.TH
import           Data.Char



dropPrefix :: String -> String -> String
dropPrefix prefix x = toLower (head rest) : tail rest
    where rest = drop (length prefix) x


deriveMyJSON :: String -> Name -> Q [ Dec ]
deriveMyJSON prefix = deriveJSON $ defaultOptions
    { fieldLabelModifier     = dropPrefix prefix
    , constructorTagModifier = map toLower
    }

