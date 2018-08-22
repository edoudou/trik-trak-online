import           Test.Tasty         (defaultMain, testGroup)
import           Test.Tasty.Hspec   (testSpec)

import qualified Data.GameSpec
import qualified GameSpec
import qualified Server.APISpec

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Server.API         (withServer, mkConfig, Handle, cEnvironnment, hConfig)
import           Server.Environment (Environment (Test))
import           Servant.Client   (mkClientEnv, parseBaseUrl, ClientEnv)
import Server.Types (port)

runSpec :: ClientEnv -> IO ()
runSpec clientEnv = do
  spec1 <- testSpec "Data.GameSpec" Data.GameSpec.spec
  spec2 <- testSpec "GameSpec" GameSpec.spec
  spec3 <- testSpec "Server.APISpec" $ Server.APISpec.spec clientEnv

  defaultMain $
    testGroup "tests"
      [ spec1
      , spec2
      , spec3
      ]

main :: IO ()
main = do
  config <- mkConfig Test
  withServer config $ \handle -> do
    clientEnv <- mkTestClientEnv handle
    runSpec clientEnv


mkTestClientEnv :: Handle -> IO ClientEnv
mkTestClientEnv handle = do
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "http://localhost:" ++ show (port (cEnvironnment (hConfig handle)))
  return $ mkClientEnv mgr baseUrl
