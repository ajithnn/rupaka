module Util where

import           Control.Exception
import           Data.Maybe         (fromMaybe)
import           Prelude            hiding (catch)
import           System.Directory
import           System.Environment
import           System.IO.Error    hiding (catch)
import           Text.Read          as TR (readMaybe)
import           Types

readNum :: String -> Int
readNum v = fromMaybe 0 $ TR.readMaybe v

extractTerms :: Config -> (Key,ValueS)
extractTerms (Config (TermS k v)) = (k,v)
extractTerms (Config (TermI k v)) = (k, show v)
extractTerms (Config (TermB k v)) = (k, show v)

readBool :: String -> Bool
readBool v  | v `elem` ["true","True"] = True
            | v `elem` ["false","False"] = False
            | otherwise = False

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
