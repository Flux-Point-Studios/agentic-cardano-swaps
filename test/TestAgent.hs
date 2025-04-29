module Main where

import CLI.Agent (runAgentMonitor)
import CLI.Types
import System.Environment (getArgs)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

-- | Create dummy key files for testing
createDummyKeyFiles :: FilePath -> IO (FilePath, FilePath)
createDummyKeyFiles dir = do
  createDirectoryIfMissing True dir
  
  let skeyFile = dir </> "test-payment.skey"
      vkeyFile = dir </> "test-payment.vkey"
      
      -- Sample dummy key content 
      skeySample = "{\"type\": \"PaymentSigningKeyShelley_ed25519\", \"description\": \"Test Key\", \"cborHex\": \"5820000000000000000000000000000000000000000000000000000000000000000000\"}"
      vkeySample = "{\"type\": \"PaymentVerificationKeyShelley_ed25519\", \"description\": \"Test Key\", \"cborHex\": \"5820000000000000000000000000000000000000000000000000000000000000000000\"}"
  
  -- Create files if they don't exist
  skeyExists <- doesFileExist skeyFile
  unless skeyExists $ BL.writeFile skeyFile $ BL.fromStrict $ TE.encodeUtf8 $ T.pack skeySample
  
  vkeyExists <- doesFileExist vkeyFile
  unless vkeyExists $ BL.writeFile vkeyFile $ BL.fromStrict $ TE.encodeUtf8 $ T.pack vkeySample
  
  putStrLn $ "Created/verified test key files in: " ++ dir
  return (skeyFile, vkeyFile)

-- | Create dummy protocol parameters file
createDummyProtocolParams :: FilePath -> IO FilePath
createDummyProtocolParams dir = do
  createDirectoryIfMissing True dir
  
  let paramsFile = dir </> "protocol-params.json"
      -- Basic protocol parameters 
      paramsJson = "{\"txFeePerByte\":44,\"minUTxOValue\":1000000,\"stakePoolDeposit\":500000000,\"utxoCostPerWord\":34482,\"collateralPercentage\":150,\"maxBlockBodySize\":90112,\"maxTxSize\":16384,\"treasuryCut\":0.2,\"minPoolCost\":340000000,\"maxCollateralInputs\":3,\"maxValueSize\":5000,\"maxTxExecutionUnits\":{\"memory\":14000000,\"steps\":10000000000},\"maxBlockExecutionUnits\":{\"memory\":62000000,\"steps\":20000000000},\"protocolVersion\":{\"minor\":0,\"major\":8},\"txFeeFixed\":155381,\"stakeAddressDeposit\":2000000,\"monetaryExpansion\":0.003,\"poolPledgeInfluence\":0.3,\"executionUnitPrices\":{\"priceSteps\":0.0000721,\"priceMemory\":0.0577}}"
  
  -- Create file if it doesn't exist
  paramsExists <- doesFileExist paramsFile
  unless paramsExists $ BL.writeFile paramsFile $ BL.fromStrict $ TE.encodeUtf8 $ T.pack paramsJson
  
  putStrLn $ "Created/verified protocol parameters in: " ++ paramsFile
  return paramsFile

main :: IO ()
main = do
  -- Get home directory for realistic paths
  home <- getHomeDirectory
  args <- getArgs
  
  -- Create test directories
  let tmpDir = home </> ".agent-test-dir"
      paramsDir = tmpDir </> "params"
      
      addrStr = if length args >= 1 then args !! 0 
                else "addr_test1vp5cxztpc6hep9s7qkjz7k0xvgc75cxj67spj72pqe37mscnhkqkk"  -- example test address
  
  -- Create dummy key files for testing
  (skeyFile, vkeyFile) <- createDummyKeyFiles tmpDir
  
  -- Create dummy protocol parameters
  paramsFile <- createDummyProtocolParams paramsDir
  
  putStrLn $ "Using address: " ++ addrStr
  
  -- Set up minimal test configuration with the API credentials
  let testConfig = AgentOptions {
        aoSigningKey = skeyFile,
        aoVerificationKey = vkeyFile,
        aoAgentAddress = addrStr,
        aoOfferAsset = ("", ""),  -- ADA
        aoAskAsset = ("1d7f33bd23d85e1a25d87d73fcf1df8d7d72b7756c165fc178fb3c1", "5457"),
        aoNetworkMagic = Just 1,  -- testnet
        aoDryRun = True,  -- Important for testing
        aoBlockfrostProjectId = Just "mainnetXdMvEPp07a5GgSWtpSqUytnmtR4OvJzr",
        aoNodeSocket = Nothing, -- Not needed with Blockfrost
        aoApiBase = "https://api.fluxpointstudios.com",  -- API endpoint
        aoApiKey = Just "16dc5e5e6671ae253eded051fab30876ca44d23c8089af7a07679528ed21eee2",
        aoPollSeconds = 5,
        aoProtocolParamsFile = Just paramsFile, -- Use our dummy params
        aoMakerOnly = True
      }
  
  putStrLn "Starting agent in test mode (dry run)..."
  putStrLn $ "API Base: " ++ aoApiBase testConfig
  putStrLn $ "API Key: " ++ take 10 (maybe "" id (aoApiKey testConfig)) ++ "..."
  
  -- Run the agent
  runAgentMonitor testConfig