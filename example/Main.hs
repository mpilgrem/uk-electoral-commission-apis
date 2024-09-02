{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Web.UkElectoralCommission.ElectionInfo.V1
                   ( Postcode (..), Token (..), getElectionInfo
                   , stdAddressPicker
                   )
import           Network.HTTP.Client ( newManager )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           System.Environment ( lookupEnv )
import           System.IO ( hFlush, stdout )

-- | This executable assumes that a valid token to use the UK's Electoral
-- Commission's API is found in environment variable
-- @UK_ELECTORAL_COMMISSION_API_TOKEN@.
main :: IO ()
main = lookupEnv "UK_ELECTORAL_COMMISSION_API_TOKEN" >>= \case
  Nothing -> T.putStrLn
    "Environment variable UK_ELECTORAL_COMMISSION_API_TOKEN not found."
  Just apiToken -> do
    T.putStr "Enter postcode: "
    hFlush stdout
    postcode' <- Postcode <$> T.getLine
    mgr <- newManager tlsManagerSettings
    let apiToken' = Token $ T.pack apiToken
    getElectionInfo mgr apiToken' stdAddressPicker postcode' >>= \case
      Nothing -> putStrLn "No result."
      Just result -> case result of
        Right response -> print response
        Left err -> putStrLn $ "Error! Result:\n" <> show err
