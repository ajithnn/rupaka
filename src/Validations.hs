
module Validations where

import           Config
import           Control.Monad
import           Data.Aeson            (encodeFile)
import           Data.ByteString       as B hiding (readFile)
import           Data.ByteString.Char8 as BC hiding (readFile)
import           Data.List             as L (foldl, replicate, zip)
import           Data.Map              as M
import           Data.Maybe
import           Language.Haskell.TH
import           System.Environment
import           Text.Read             as T (readMaybe)
import           Types

validate :: [Validation] -> [Config] -> Either String Bool
validate validations configs = validated
  where cmap = M.fromList $ Prelude.map (\(Config (Term k v)) -> (k,v)) configs
        validated = Prelude.foldl (\acc v -> combineValidations (validator' v cmap) acc) (Right True) validations

combineValidations :: Either String Bool -> Either String Bool -> Either String Bool
combineValidations _ (Left error)      = Left error
combineValidations (Right v) (Right b) = Right (v && b)
combineValidations (Left error) _      = Left error

readValue :: Value -> Int
readValue v = fromMaybe 0 $ T.readMaybe v

validator' :: Validation -> Map Key Value -> Either String Bool
validator' (Validation k c v) cfgM = resp
  where res = getOperator c (readValue (cfgM ! k)) (readValue v)
        resp  | res = Right True
              | otherwise = Left $ mconcat ["Validation failed for: ", k , show c, v]

getOperator :: Condition -> (Int -> Int -> Bool)
getOperator CGT  = (>)
getOperator CLT  = (<)
getOperator CLE  = (<=)
getOperator CGE  = (>=)
getOperator CEQ  = (==)
getOperator None = \_ _ -> False


