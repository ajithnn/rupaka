module Validations where

import           Config
import           Control.Monad
import           Data.Aeson            (encodeFile)
import           Data.ByteString       as B hiding (readFile)
import           Data.ByteString.Char8 as BC hiding (readFile)
import           Data.List             as L (all, elem, foldl, map, replicate,
                                             zip)
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
  where validated = Prelude.foldl (\acc v -> combineValidations (validator v configs) acc) (Right True) validations

combineValidations :: Either String Bool -> Either String Bool -> Either String Bool
combineValidations (Left er) (Left e)  = Left (mconcat [er, " \n",e])
combineValidations _ (Left error)      = Left error
combineValidations (Left error) _      = Left error
combineValidations (Right v) (Right b) = Right (v && b)

validator :: Validation -> [Config] -> Either String Bool
validator vld cfgM = resp
  where res = case vld of
                (ValidationS  k MATCHES v)  -> extractString (getPair cfgM k) TRT.=~ v :: Bool
                (ValidationB k IS v)        -> extractBool (getPair cfgM k) == v
                (ValidationI k c v)         -> getOperator c (extractNum (getPair cfgM k)) v
                (ValidationS  k ONEOF v)    -> extractString (getPair cfgM k) `L.elem` splitOn "|" v
                (ValidationSA k ONEOF v)    -> L.all (\vl -> vl `L.elem` splitStr v) (extractStrings (getPair cfgM k))
                (ValidationIA k c v)        -> L.all (\vl -> getOperator c vl v) (extractNums (getPair cfgM k))
                (ValidationBA k ONEOF v)    -> L.all (`L.elem` v) (extractBools (getPair cfgM k))
                (ValidationSA k MATCHES v)  -> L.all (\vl -> vl TRT.=~ v :: Bool) (extractStrings (getPair cfgM k))
                _ -> False
        splitStr = splitOn "|"
        resp  | res = Right True
              | otherwise = Left $ mconcat ["Failed - ", show vld]

getPair :: [Config] -> Key -> Config
getPair cfgs ky = pairs
  where pairs = Prelude.head $ Prelude.filter (\(Config pair) -> case pair of
                                          TermS k v  -> ky == k
                                          TermI k v  -> ky == k
                                          TermB k v  -> ky == k
                                          TermSA k v -> ky == k
                                          TermBA k v -> ky == k
                                          TermIA k v -> ky == k
                                          ) cfgs

getOperator :: Condition -> (Double -> Double -> Bool)
getOperator CGT = (>)
getOperator CLT = (<)
getOperator CLE = (<=)
getOperator CGE = (>=)
getOperator CEQ = (==)
getOperator _   = \_ _ -> False

