-- |
--
-- Module      :  Exec
-- Description :  Executes 'Command's.
-- License     :  MIT
--
-- Executes 'Command's.
--

module Exec ( execCommand, execConfigs ) where

import ExecConfig

import System.Process


-----------------------------------------------------------------------------

-- | Execute a 'Command'.
execCommand :: Command -> IO()
execCommand (Cmd cmd) = do
    putStrLn $ "Executing: " ++ cmd
    callCommand cmd

-- | Execute the 'Command's, build from the given configs.
execConfigs :: [ExecConfig] -> [ExecParams] -> DataConfig -> IO ()
execConfigs e p d = do
    callCommand $ "mkdir -p " ++ logDir d
    mapM_ execCommand $ createCommands e p d
