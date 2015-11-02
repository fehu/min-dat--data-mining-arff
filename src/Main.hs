-- |
--
-- Module      :  Main
-- License     :  MIT
--
-- Usage: DataMiningWeka [-h] exec-conf param-conf data-conf
--
--       exec-conf  | applications YAML configuration
--
--       param-conf | applications parameters YAML configuration
--
--       data-conf  | data files YAML configuration


module Main ( main ) where

import Exec
import ExecConfig

import Data.Maybe (fromMaybe)

import System.Environment
import System.Exit

-----------------------------------------------------------------------------

main = getArgs >>= parse

parse ["-h"] = usage >> exitSuccess

parse [execf, paramf, dataf] = do

    let err x = error $ "Couldn't read " ++ x ++ " configuration"
    let get s = fmap (fromMaybe (err s))

    execConfs  <- get "exec"  $ readExecConfigs execf
    paramConfs <- get "param" $ readExecParams  paramf
    dataConf   <- get "data"  $ readDataConfig  dataf

    execConfigs execConfs paramConfs dataConf


parse _ = unknownCmd >> usage >> exitFailure

unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: DataMiningWeka [-h] exec-conf param-conf data-conf\n"
           putStrLn "       exec-conf  | applications YAML configuration"
           putStrLn "       param-conf | applications parameters YAML configuration"
           putStrLn "       data-conf  | data files YAML configuration"

