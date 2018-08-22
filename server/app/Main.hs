module Main where

import           Control.Monad      (void)

import           Server.API         (runServer)
import           Server.Environment (Environment (Prod))

main :: IO ()
main = void $ runServer Prod
