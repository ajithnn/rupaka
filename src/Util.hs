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

extractBool (Boolean k v) = v
extractBool _             = False

extractKey (Str k v)      = k
extractKey (Strs k v)     = k
extractKey (Numeric k v)  = k
extractKey (Numerics k v) = k
extractKey (Boolean k v)  = k
extractKey (Booleans k v) = k
extractKey (CObject k v)  = k
extractKey (CObjects k v) = k

extractObjects ky (CObject k v)  = [v | ky == k]
extractObjects ky (CObjects k v)  | ky == k = v
                                  | otherwise = []
extractObjects _ _              = []

extractStrings (Strs k v) = v
extractStrings (Str k v) = [v]
extractStrings _          = []

extractNums (Numerics k v) = v
extractNums (Numeric k v)  = [v]
extractNums _              = []

extractBools (Booleans k v) = v
extractBools _              = []

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
