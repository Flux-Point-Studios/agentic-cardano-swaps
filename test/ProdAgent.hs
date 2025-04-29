module Main where

import CLI.PatchedAgent (runAgentMonitor)
import CLI.Types
import System.Environment (lookupEnv)
import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromMaybe, isNothing)

-- | Production agent entry point
main :: IO ()
main = do
  -- Gather environment variables
  mSigningKey <- lookupEnv "AGENT_SKEY"
  mVerificationKey <- lookupEnv "AGENT_VKEY" 
  mAddress <- lookupEnv "AGENT_ADDRESS"
  mBlockfrost <- lookupEnv "BLOCKFROST_PROJECT_ID"
  mParams <- lookupEnv "PROTOCOL_PARAMS"
  mOpenAIKey <- lookupEnv "OPENAI_API_KEY"
  
  -- Check for required configuration
  let missingVars = 
        (if isNothing mSigningKey then ["AGENT_SKEY"] else []) ++
        (if isNothing mVerificationKey then ["AGENT_VKEY"] else []) ++
        (if isNothing mAddress then ["AGENT_ADDRESS"] else []) ++
        (if isNothing mBlockfrost then ["BLOCKFROST_PROJECT_ID"] else []) ++
        (if isNothing mParams then ["PROTOCOL_PARAMS"] else []) ++
        (if isNothing mOpenAIKey then ["OPENAI_API_KEY"] else [])
  
  when (not (null missingVars)) $ do
    hPutStrLn stderr $ "Error: Missing required environment variables: " ++ show missingVars
    exitFailure
  
  -- Set up agent configuration with real credentials from environment
  let signingKey = fromMaybe "" mSigningKey
      verificationKey = fromMaybe "" mVerificationKey
      agentAddress = fromMaybe "" mAddress
      blockfrostProjectId = fromMaybe "" mBlockfrost
      paramsFile = fromMaybe "" mParams
      openaiKey = fromMaybe "" mOpenAIKey
  
  -- Set trading pair for ADA <-> SUNDAE
  let adaAsset = ("", "")  -- Empty strings represent ADA
      tokenAsset = ("9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77", "SUNDAE")
      -- Use our real OpenAI key for improved reasoning
      useImprovedAI = True
  
  putStrLn "====================================================================="
  putStrLn "Starting Cardano Swaps Agent in Production Mode"
  putStrLn "====================================================================="
  putStrLn $ "Address: " ++ agentAddress
  putStrLn $ "Trading Pair: ADA <-> SUNDAE"
  putStrLn $ "Using Blockfrost for chain access"
  putStrLn $ "Using improved OpenAI reasoning model"
  putStrLn "====================================================================="
  
  -- Production configuration
  let prodConfig = AgentOptions {
        aoSigningKey = signingKey,
        aoVerificationKey = verificationKey,
        aoAgentAddress = agentAddress,
        aoOfferAsset = adaAsset,  -- ADA
        aoAskAsset = tokenAsset,  -- Token (SUNDAE in this example)
        aoNetworkMagic = Nothing,  -- Mainnet (no magic number)
        aoDryRun = False,  -- Run for real
        aoBlockfrostProjectId = Just blockfrostProjectId,
        aoNodeSocket = Nothing, -- Not needed with Blockfrost
        aoApiBase = if useImprovedAI
                    then "https://api.openai.com/v1/chat/completions"  -- Direct OpenAI access
                    else "https://api.fluxpointstudios.com",  -- Default API gateway
        aoApiKey = Just openaiKey,  -- Use our OpenAI key
        aoPollSeconds = 60,  -- Poll every minute
        aoProtocolParamsFile = Just paramsFile,
        aoMakerOnly = False  -- Allow both maker and taker actions
      }
  
  -- Run the agent
  runAgentMonitor prodConfig 