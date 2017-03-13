{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Either
import qualified Network.Fastly as F
import qualified System.Environment as Env
import qualified Data.Text as T

surrogateKey = F.SurrogateKey "example/1"

testServiceResult token serviceId = do
  r <- F.fastly token (\client -> F.getService client serviceId)
  putStrLn $ "\ngetService: " ++ show r ++ "\n"
  return r

testPurgeKeyResult token serviceId = do
  r <- F.fastly token (\client -> F.purgeKey client F.Instant serviceId surrogateKey)
  putStrLn $ "\npurgeKey: " ++ show r ++ "\n"
  return r

purgeOk (Right (F.PurgeResult {F.purgeResultStatus = "ok", F.purgeResultId = _})) = True
purgeOk _ = False

tests token serviceId = do
  getServiceResult <- testServiceResult token serviceId
  purgeKeyResult <- testPurgeKeyResult token serviceId
  hspec $ do
    describe "getService" $ do
      it "is okay" $ do
        getServiceResult `shouldSatisfy` isRight
    describe "purgeKey" $ do
      it "is okay" $ do
        purgeKeyResult `shouldSatisfy` purgeOk

main :: IO ()
main = do
  token <- Env.getEnv "FASTLY_TOKEN"
  serviceId <- Env.getEnv "FASTLY_SERVICE_ID"
  tests (T.pack token) (F.ServiceId (T.pack serviceId))
