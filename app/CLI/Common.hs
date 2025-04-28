module CLI.Common
(
  -- Re-exports from Types that both Agent and Run need
  module CLI.Types,
  
  -- Functions and types shared between Agent and Run
  runAgentMonitor
) where

import CLI.Types

-- Function signature that both modules can reference
-- The implementation will be in Agent.hs, but the type declaration in Common
runAgentMonitor :: AgentOptions -> IO () 