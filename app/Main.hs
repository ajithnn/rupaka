{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Config
import           Data.Aeson
import           Data.ByteString       as B hiding (readFile)
import           Data.ByteString.Char8 as BC hiding (readFile)
import           Data.Map              as M
import           Data.Maybe
import           System.Environment
import           Text.Read             as T (readMaybe)


main :: IO ()
main = do
  args <- getArgs
  case validate cfgs of
    Right val -> do
      encodeFile (Prelude.head args) val
      print "Generated Json Configurations"
    Left e -> print $ mconcat ["Validations failed for: ",e]

cfgs :: [Config]
cfgs = $(configsFromFile Config.filePath)

validate :: [Config] -> Either String (Map Config.Key Config.Value)
validate c = result
  where cmapL = Prelude.filter (\(KvPair (k,v)) -> k /= "" ) (Prelude.map extractTerms c)
        cmapV = Prelude.filter (\(k,c,v) -> k /= "" ) (Prelude.map extractValidations c)
        cmap = M.fromList (Prelude.map (\(KvPair (k,v)) -> (k,v)) cmapL)
        validated = Prelude.foldl (\a v -> validation (runValidate v cmap) a) (Right True) cmapV
        result = case validated of
                  Left e  -> Left e
                  Right b -> if b then Right cmap else Left "Unknown error"

validation :: Either String Bool -> Either String Bool -> Either String Bool
validation _ (Left error)      = Left error
validation (Right v) (Right b) = Right (v && b)
validation (Left error) _      = Left error

extractTerms :: Config -> KvPair
extractTerms (Config (Term k v)) = KvPair (k,v)
extractTerms _                   = KvPair ("","")

runValidate :: (Config.Key,Condition,Config.Key) -> Map Config.Key Config.Value -> Either String Bool
runValidate (vk,c,vl) cm = response
  where res = case c of
                CGT  -> value > limit
                CLT  -> value < limit
                CGE  -> value >= limit
                CLE  -> value <= limit
                CEQ  -> value == limit
                None -> False
        value = fromMaybe 0 $ T.readMaybe $ cm ! vk
        limit = fromMaybe 0 $ T.readMaybe vl
        response  | res = Right True
                  | otherwise = Left (vk++" "++ show c ++" "++vl)

extractValidations :: Config -> (Config.Key,Condition,Config.Value)
extractValidations (Config (Validation k c v)) = (k,c,v)
extractValidations _                           = ("",None,"")
