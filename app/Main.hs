{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Config
import           Data.Aeson                 (encodeFile)
import           Data.ByteString            as B hiding (readFile)
import           Data.ByteString.Char8      as BC hiding (readFile)
import           Data.Map                   as M
import           Data.Maybe
import           Language.Haskell.TH.Syntax
import           System.Environment
import           Text.Read                  as T (readMaybe)
import           Types
import           Validations

validations :: [Validation]
validations = $(validationsFromFile Config.filePath)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = Prelude.head args
  let cfgPath = args !! 1
  configs <- compileConfig cfgPath
  case configs of
    Left e -> print e
    Right cfgs ->
      case validate validations cfgs of
        Right _ -> do
          encodeFile outputPath $ M.fromList (Prelude.map (\(Config (Term k v)) -> (k,v)) cfgs)
          print "Generated Json Configurations"
        Left e -> print e
