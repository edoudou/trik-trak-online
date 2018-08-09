{-# LANGUAGE OverloadedStrings #-}

module Server.Types where

import           Data.Aeson

data Valid e a = KO e | OK a
  deriving (Eq, Show)

eitherToValid :: Either e a -> Valid e a
eitherToValid (Left e)  = KO e
eitherToValid (Right a) = OK a

instance (ToJSON e, ToJSON a) => ToJSON (Valid e a) where
  toJSON (KO e) = object ["error" .= toJSON e]
  toJSON (OK a) = object ["success" .= toJSON a]
