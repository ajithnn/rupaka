module Util where

import           Data.Maybe (fromMaybe)
import           Text.Read  as TR (readMaybe)
import           Types

readNum :: String -> Int
readNum v = fromMaybe 0 $ TR.readMaybe v

extractTerms :: Config -> (Key,ValueS)
extractTerms (Config (TermS k v)) = (k,v)
extractTerms (Config (TermI k v)) = (k, show v)
