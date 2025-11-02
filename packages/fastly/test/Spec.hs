{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Either
import qualified Network.Fastly as F
import qualified System.Environment as Env
import qualified Data.Text as T

surrogateKey = F.SurrogateKey "example/1"

testService token serviceId = do
  r <- F.fastly token (\client -> F.getService client serviceId)
  putStrLn $ "\ngetService: " ++ show r ++ "\n"
  return r

testPurgeKey token serviceId = do
  r <- F.fastly token (\client -> F.purgeKey client F.Instant serviceId surrogateKey)
  putStrLn $ "\npurgeKey: " ++ show r ++ "\n"
  return r

testPurgeAll token serviceId = do
  r <- F.fastly token $ \client -> F.purgeAll client serviceId
  putStrLn $ "\npurgeAll: " ++ show r ++ "\n"
  return r

purgeKeyOk (Right (F.PurgeResult {F.purgeResultStatus = "ok", F.purgeResultId = _})) = True
purgeKeyOk _ = False

purgeAllOk (Right (F.PurgeAllResult {F.purgeAllResultStatus = "ok"})) = True
purgeAllOk _ = False

tests token serviceId = do
  getServiceResult <- testService token serviceId
  purgeKeyResult <- testPurgeKey token serviceId
  purgeAllResult <- testPurgeAll token serviceId
  hspec $ do
    describe "getService" $ do
      it "is okay" $ do
        getServiceResult `shouldSatisfy` isRight
    describe "purgeKey" $ do
      it "is okay" $ do
        purgeKeyResult `shouldSatisfy` purgeKeyOk
    describe "purgeAll" $ do
      it "is okay" $ do
        purgeAllResult `shouldSatisfy` purgeAllOk

main :: IO ()
main = do
  token <- Env.getEnv "FASTLY_TOKEN"
  serviceId <- Env.getEnv "FASTLY_SERVICE_ID"
  tests (T.pack token) (F.ServiceId (T.pack serviceId))
