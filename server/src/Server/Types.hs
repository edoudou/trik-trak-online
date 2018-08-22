{-# LANGUAGE OverloadedStrings #-}

module Server.Types
  ( Port
  , Valid(KO, OK)
  , valid
  , invalid
  , eitherToValid
  , port
  )
  where

import           Data.Aeson

import Server.Environment (Environment(..))

type Port = Int

data Valid e a = KO e | OK a
  deriving (Eq, Show)

valid :: a -> Valid e a
valid = OK

invalid :: e -> Valid e a
invalid = KO

eitherToValid :: Either e a -> Valid e a
eitherToValid (Left e)  = KO e
eitherToValid (Right a) = OK a

instance (ToJSON e, ToJSON a) => ToJSON (Valid e a) where
  toJSON (KO e) = object ["error" .= toJSON e]
  toJSON (OK a) = object ["success" .= toJSON a]

port :: Environment -> Port
port Prod = 8090
port Dev  = 8091
port Test = 8092
