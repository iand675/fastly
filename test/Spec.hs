{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Fastly as F
import qualified System.Environment as Env
import qualified Data.Text as T
import qualified Data.Text.IO as T

testGetService token serviceId = do
  r <- F.fastly token (\client -> F.getService client serviceId)
  case r of
    Left err ->
      T.putStrLn (T.intercalate " " ["Error: ", T.pack (show err)])
    Right service ->
      T.putStrLn (T.intercalate " " ["Service name: ", F.serviceName service])

tests token serviceId = do
  testGetService token serviceId

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [token, serviceId] -> tests (T.pack token) (F.ServiceId (T.pack serviceId))
    _                  -> do
      progName <- Env.getProgName
      putStrLn ("usage: " ++ progName ++ " <token> <serviceId>")
