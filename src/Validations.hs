module Validations where

import           Config
import           Control.Monad
import           Data.Aeson            (encodeFile)
import           Data.ByteString       as B hiding (readFile)
import           Data.ByteString.Char8 as BC hiding (readFile)
import           Data.List             as L (elem, foldl, replicate, zip)
import           Data.List.Split
import           Data.Map              as M
import           Data.Maybe
import           Language.Haskell.TH
import           System.Environment
import           Text.Read             as T (readMaybe)
import qualified Text.Regex.Base       as TRB
import qualified Text.Regex.TDFA       as TRT ((=~))
import           Types
import           Util

validate :: Either String [Validation] -> Either String [Config] -> Either String Bool
validate (Left er) (Left e) = Left (mconcat [er," \n",e])
validate (Left e) _ = Left e
validate _ (Left e) = Left e
validate (Right validations) (Right configs) = validated
  where cmap = M.fromList $ Prelude.map extractTerms configs
        validated = Prelude.foldl (\acc v -> combineValidations (validator' v cmap) acc) (Right True) validations

combineValidations :: Either String Bool -> Either String Bool -> Either String Bool
combineValidations (Left er) (Left e)  = Left (mconcat [er, " \n",e])
combineValidations _ (Left error)      = Left error
combineValidations (Left error) _      = Left error
combineValidations (Right v) (Right b) = Right (v && b)

validator' :: Validation -> Map Key ValueS -> Either String Bool
validator' vld cfgM = resp
  where res = case vld of
                (ValidationS k MATCHES v) -> (cfgM ! k) TRT.=~ v :: Bool
                (ValidationS k ONEOF v)   -> (cfgM ! k) `L.elem` splitOn "|" v
                (ValidationB k IS v)      -> readBool (cfgM ! k) == v
                (ValidationI k c v)       -> getOperator c (readNum (cfgM ! k)) v
                _ -> False
        resp  | res = Right True
              | otherwise = Left $ mconcat ["Failed - ", show vld]

getOperator :: Condition -> (Int -> Int -> Bool)
getOperator CGT = (>)
getOperator CLT = (<)
getOperator CLE = (<=)
getOperator CGE = (>=)
getOperator CEQ = (==)
getOperator _   = \_ _ -> False

