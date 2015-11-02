{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Module      :  ExecConfig
-- Copyright   :
-- License     :  MIT
--
--
--


module ExecConfig (

  ExecConfig(..)
, OptParam(..)

, ExecParams(..)

, DataFile, ClassName, LogDir
, Command(..)
, PrepareCommand(..)
, createCommand
, createCommands

) where

import Data.Yaml
import Data.Maybe (fromMaybe)
import Data.List  (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Txt
import Control.Applicative ( (<$>), (<*>) )
import Control.Exception ( assert )
import System.FilePath


-----------------------------------------------------------------------------

type DataFile  = String
type ClassName = String
type LogDir    = String

newtype Command = Cmd String deriving (Show, Eq, Ord)

newtype PrepareCommand = PrepareCmd (DataFile -> ClassName -> LogDir -> Command)


createCommand :: ExecConfig -> Maybe ExecParams -> PrepareCommand

createCommand exc (Just prms@(ExecParams nme params')) = assert (execName exc == nme)
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


createCommand exc _ = PrepareCmd . mkCommand exc
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
                      (Txt.pack $ logdir ++ pathSeparator:execName exc)
        $ txt


createCommands :: [ExecConfig] -> [ExecParams] -> [PrepareCommand]
createCommands econfs eparams = do
    econf <- econfs
    let eparam = find (\(ExecParams nme _) -> nme == execName econf) eparams
    return $ createCommand econf eparam

-----------------------------------------------------------------------------


data OptParam = OptParam { optParamName    :: String
                         , optParamStr     :: String
                         , optParamReplace :: String
                         }
              deriving Show

data ExecConfig = ExecConfig { execName  :: String
                             , execStr   :: String
                             , params    :: [String]
                             , optParams :: [OptParam]
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


data ExecParams = ExecParams String (Map String String) deriving Show


instance FromJSON ExecParams where
    parseJSON (Object v) = ExecParams   <$>
                           v .: "name"  <*>
                           v .: "params"

-----------------------------------------------------------------------------


data DataConfig = DataConfig { datafile  :: String
                             , classname :: String
                             , logDir    :: String
                             }

instance FromJSON DataConfig where
    parseJSON (Object v) = DataConfig   <$>
                           v .: "data"  <*>
                           v .: "class" <*>
                           v .: "report-dir"

-----------------------------------------------------------------------------

