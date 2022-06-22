{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Exception
import           Data.Maybe         (fromMaybe)
import           Data.Text          as T (pack, replace, splitOn, unpack)
import           Prelude            hiding (catch)
import           System.Directory
import           System.Environment
import           System.IO.Error    hiding (catch)
import           Text.Read          as TR (readMaybe)
import           Types

extractBool :: Config -> Bool
extractBool (Config (TermB k v)) = v
extractBool _                    = False

extractNum :: Config -> Double
extractNum (Config (TermI k v)) = v
extractNum _                    = 0

extractString :: Config -> String
extractString (Config (TermS k v)) = v
extractString _                    = ""

readNum :: String -> Double
readNum v = fromMaybe 0 $ TR.readMaybe v

readNums :: [String] -> [Double]
readNums = map readNum

readBool :: String -> Bool
readBool v  | v `elem` ["true","True"] = True
            | v `elem` ["false","False"] = False
            | otherwise = False

readBools :: [String] -> [Bool]
readBools = map readBool

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
