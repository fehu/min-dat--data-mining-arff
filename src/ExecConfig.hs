{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Module      :  ExecConfig
-- Description :  Applications execution configs.
-- License     :  MIT
--
-- Builds commands to execute applications, given configs:
-- @
--  [ExecConfig] -> [ExecParams] -> DataConfig -> [Command]
-- @


module ExecConfig (

-- * Execution config
  ExecConfig(..)
, OptParam(..)

-- * Parameters config
, ExecParams(..)

-- * Prepare for data
, DataFile, ClassName, LogDir
, PrepareCommand(..)
, prepareCommand
, prepareCommands

-- * Data config
, DataConfig(..)

-- * 'PrepareCommand' -> 'Command'
, Command(..)
, createCommand
, createCommands

-- * Read Yaml
, Filename
, readExecConfigs
, readExecParams
, readDataConfig

) where

import Data.Yaml
import Data.Maybe (fromMaybe)
import Data.List  (find)
import Data.Map (Map)

import qualified Data.Map as Map
import qualified Data.Text as Txt
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ( (<$>), (<*>) )
import Control.Exception ( assert )
import Control.Monad ( liftM )
import System.FilePath


-----------------------------------------------------------------------------

type DataFile  = String
type ClassName = String
type LogDir    = String

-- | A string to be executed as a system command.
newtype Command = Cmd String deriving (Show, Eq, Ord)

-- | A container for the final step of 'Command' creation ('DataConfig' -> ...).
newtype PrepareCommand = PrepareCmd (DataFile -> ClassName -> LogDir -> Command)

-- | Substitutes params in 'ExecConfig' by those defined in 'ExecParams'
--   (except $file, $class and $log).
prepareCommand :: ExecConfig -> Maybe ExecParams -> PrepareCommand

prepareCommand exc (Just prms@(ExecParams nme params')) = assert (execName exc == nme)
    $ PrepareCmd cmd
    where paramsReplace = do param <- params exc
                             let err = error "not found parameter '" ++ show param
                                     ++ "' in " ++ show prms
                             let v = fromMaybe err $ Map.lookup param params'
                             return (Txt.pack $ "$" ++ param, Txt.pack v)
          optParamsReplace = do
                         OptParam pname pstr prepl <- optParams exc
                         let v  = fromMaybe "" $ Map.lookup pname params'
                         let v' = Txt.replace (Txt.pack prepl) (Txt.pack v) (Txt.pack pstr)
                         return (Txt.pack $ "$" ++ pname, v')

          cmd = mkCommand exc $ txtReplace (paramsReplace ++ optParamsReplace)
                                           (execStr exc)


prepareCommand exc _ = PrepareCmd . mkCommand exc
                    . txtReplace optParamsReplace $ execStr exc
    where optParamsReplace = do OptParam pname _ _ <- optParams exc
                                return (Txt.pack $ "$" ++ pname, Txt.empty)


txtReplace replaces init = let fs = do (target, repl) <- replaces
                                       return $ Txt.replace target repl
                          in foldr (.) id fs $ Txt.pack init

mkCommand exc txt dfile clazz logdir =
    Cmd . Txt.unpack
        . Txt.replace (Txt.pack "$file")  (Txt.pack dfile)
        . Txt.replace (Txt.pack "$class") (Txt.pack clazz)
        . Txt.replace (Txt.pack "$log")
                      (Txt.pack $ logdir ++ pathSeparator:execName exc ++ ".log")
        $ txt


-- | Creates 'PrepareCommand's using ['ExecConfig'] and ['ExecParams'].
prepareCommands :: [ExecConfig] -> [ExecParams] -> [PrepareCommand]
prepareCommands econfs eparams = do
    econf <- econfs
    let eparam = find (\(ExecParams nme _) -> nme == execName econf) eparams
    return $ prepareCommand econf eparam

-----------------------------------------------------------------------------

-- | An optional application parameter.
data OptParam = OptParam { optParamName    :: String -- ^ parameter's name.
                         , optParamStr     :: String -- ^ parameter's string
                                                     --   representation with a
                                                     --   variable to be replaced.
                         , optParamReplace :: String -- ^ variable to replace.
                         }
              deriving Show

-- | Main application execution config.
data ExecConfig = ExecConfig { execName  :: String      -- ^ Application name.
                             , execStr   :: String      -- ^ Executable string with
                                                        --   variables to be replaced.
                             , params    :: [String]    -- ^ obligatory parameters' names.
                             , optParams :: [OptParam]  -- ^ optonal parameters,
                                                        --   see 'OptParam'.
                             }
                deriving Show



instance FromJSON OptParam where
    parseJSON (Object v) = OptParam <$>
                        v .: "name" <*>
                        v .: "str"  <*>
                        v .: "replace"



instance FromJSON ExecConfig where
    parseJSON (Object v) = ExecConfig               <$>
                           v .: "name"              <*>
                           v .: "exec"              <*>
                           v .:? "params" .!= []    <*>
                           v .:? "opt-params" .!= []

    parseJSON _ = error "Can't parse ExecConfig from YAML/JSON"

-----------------------------------------------------------------------------

-- | Execution parameters: application name and name->value 'Map'.
data ExecParams = ExecParams String (Map String String)
                deriving Show


instance FromJSON ExecParams where
    parseJSON (Object v) = ExecParams   <$>
                           v .: "name"  <*>
                           v .: "params"

-----------------------------------------------------------------------------

-- | Data configuration.
data DataConfig = DataConfig { datafile  :: String -- ^ *.arff data file path.
                             , classname :: String -- ^ class name.
                             , logDir    :: String -- ^ log directory.
                             }

instance FromJSON DataConfig where
    parseJSON (Object v) = DataConfig   <$>
                           v .: "data"  <*>
                           v .: "class" <*>
                           v .: "report-dir"

-----------------------------------------------------------------------------

finalCommand :: DataConfig -> PrepareCommand -> Command
finalCommand dconf (PrepareCmd cmd) = cmd (datafile  dconf)
                                          (classname dconf)
                                          (logDir    dconf)

createCommand :: ExecConfig -> Maybe ExecParams -> DataConfig -> Command
createCommand ec ep dc = finalCommand dc $ prepareCommand ec ep

createCommands :: [ExecConfig] -> [ExecParams] -> DataConfig -> [Command]
createCommands ecs eps dc = map (finalCommand dc) $ prepareCommands ecs eps

-----------------------------------------------------------------------------

type Filename = String

readExecConfigs :: Filename -> IO (Maybe [ExecConfig])
readExecParams  :: Filename -> IO (Maybe [ExecParams])
readDataConfig  :: Filename -> IO (Maybe DataConfig)

readExecConfigs = liftM decode . BS.readFile
readExecParams  = liftM decode . BS.readFile
readDataConfig  = liftM decode . BS.readFile

-----------------------------------------------------------------------------
