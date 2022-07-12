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

extractBool (Just (Boolean k v)) = v
extractBool _                    = False

extractNum (Just (Numeric k v)) = v
extractNum _                    = 0

extractString (Just (Str k v)) = v
extractString _                = ""

extractStrings (Just (Strs k v)) = v
extractStrings _                 = []

extractNums (Just (Numerics k v)) = v
extractNums _                     = []

extractBools (Just (Booleans k v)) = v
extractBools _                     = []

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
