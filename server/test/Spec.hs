import           Test.Tasty              (defaultMain, testGroup)
import           Test.Tasty.Hspec        (testSpec, hspec)

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)

import qualified Data.GameSpec
import qualified GameSpec
import           Server.API              (Config, Handle, cEnvironnment, hConfig,
                                          fetchConfig, withServer)
import qualified Server.APISpec
import           Server.Environment      (Environment (Test))
import           Server.Types            (port)


runSpec :: Handle -> ClientEnv -> IO ()
runSpec handle clientEnv = do
  spec1 <- testSpec "Data.GameSpec" Data.GameSpec.spec
  spec2 <- testSpec "GameSpec" GameSpec.spec
  spec3 <- testSpec "Server.APISpec" $ Server.APISpec.spec handle clientEnv

  hspec $ Server.APISpec.spec handle clientEnv

  -- TODO: concurrency issue with Tasty and Hspec
  -- Cannot add spec3 in there because concurrency is messed up
  defaultMain $
    testGroup "tests"
      [ spec1
      , spec2
      ]

main :: IO ()
main = do
  config <- fetchConfig Test
  withServer config $ \h -> do
    clientEnv <- mkTestClientEnv config
    runSpec h clientEnv

mkTestClientEnv :: Config -> IO ClientEnv
mkTestClientEnv config = do
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "http://localhost:" ++ show (port (cEnvironnment config))
  return $ mkClientEnv mgr baseUrl
