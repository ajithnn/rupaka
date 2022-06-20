{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Data.Aeson                 (encodeFile)
import           Data.ByteString            as B hiding (readFile)
import           Data.ByteString.Char8      as BC hiding (readFile)
import           Data.Either
import           Data.Map                   as M
import           Data.Maybe
import           Data.Semigroup             ((<>))
import           Language.Haskell.TH.Syntax
import           Options.Applicative
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Read                  as T (readMaybe)
import           Types
import           Util
import           Validations

main :: IO ()
main = handleInput =<< execParser opts
  where opts = info (inputOptions <**> helper)
                ( fullDesc
                <> progDesc "Parse a config and validation file and return errors or JSON"
                <> header "rupaka - Config parser / validator" )

inputOptions :: Parser InputOptions
inputOptions = InputOptions
                <$> strOption
                    ( long "config"
                    <> short 'c'
                    <> metavar "CONFIG FILE PATH"
                    <> help "Filepath for the config file to parse" )
                <*> strOption
                    ( long "validation"
                    <> short 'v'
                    <> metavar "VALIDATION FILE PATH"
                    <> help "Filepath for validation file to parse")
                <*> strOption
                    ( long "output"
                    <> short 'o'
                    <> metavar "OUTPUT FILE PATH"
                    <> help "Filepath for output file" )

handleInput :: InputOptions -> IO()
handleInput (InputOptions cfgPath vldPath outPath) = do
  removeIfExists "parse.err"
  eh <- fileHandler "parse.err" INFO
  updateGlobalLogger "rupaka.Main.Err" (addHandler eh)
  validations <- compileValid vldPath
  configs <- compileConfig cfgPath
  case validate validations configs of
    Right _ -> do
      encodeFile outPath (Output (fromRight [] configs))
      print "Generated Json Configurations"
    Left e -> errorM "rupaka.Main.Err" e
