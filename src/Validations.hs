module Validations where

import           Control.Monad
import           Data.Aeson            (encodeFile)
import           Data.ByteString       as B hiding (readFile)
import           Data.ByteString.Char8 as BC hiding (readFile)
import           Data.List             as L (all, elem, foldl, map, replicate,
                                             zip)
import           Data.List.Split
import           Data.Map              as M
import           Data.Maybe            as Mb
import           Language.Haskell.TH
import           Parser
import           System.Environment
import           Text.Read             as T (readMaybe)
import qualified Text.Regex.Base       as TRB
import qualified Text.Regex.TDFA       as TRT ((=~))
import           Types
import           Util

validate :: Either String VldTriples -> Either String ConfigPairs -> Either String Bool
validate (Left er) (Left e) = Left (mconcat [er," \n",e])
validate (Left e) _ = Left e
validate _ (Left e) = Left e
validate (Right (VldTriples validations)) (Right (ConfigPairs configs)) = validated
  where validated = Prelude.foldl (\acc v -> combineValidations (validator v configs) acc) (Right True) validations

combineValidations :: Either String Bool -> Either String Bool -> Either String Bool
combineValidations (Left er) (Left e)  = Left (mconcat [er, " \n",e])
combineValidations _ (Left error)      = Left error
combineValidations (Left error) _      = Left error
combineValidations (Right v) (Right b) = Right (v && b)

validator :: Triple -> [Pair] -> Either String Bool
validator vld cfgM = resp
  where res = case vld of
                (VStr  k MATCHES v)   -> extractString (getPair cfgM k) TRT.=~ v :: Bool
                (VBoolean k IS v)     -> extractBool (getPair cfgM k) == v
                (VNumeric k c v)      -> getOperator c (extractNum (getPair cfgM k)) v
                (VStr  k ONEOF v)     -> extractString (getPair cfgM k) `L.elem` splitOn "|" v
                (VStrs k ONEOF v)     -> L.all (\vl -> vl `L.elem` splitStr v) (extractStrings (getPair cfgM k))
                (VNumerics k c v)     -> L.all (\vl -> getOperator c vl v) (extractNums (getPair cfgM k))
                (VBooleans k ONEOF v) -> L.all (`L.elem` v) (extractBools (getPair cfgM k))
                (VStrs k MATCHES v)   -> L.all (\vl -> vl TRT.=~ v :: Bool) (extractStrings (getPair cfgM k))
                _ -> False
        splitStr = splitOn "|"
        resp  | res = Right True
              | otherwise = Left $ mconcat ["Failed - ", show vld]

getPair :: [Pair] -> Key -> Maybe Pair
getPair cfgs ky = firstOf filteredPairs
  where firstOf []     = Nothing
        firstOf (x:xs) = Just x
        pairs [] p        = p
        pairs (k:ks) cfs = pairs ks (fromMaybe [] $ firstOf $ Mb.mapMaybe (filterPairs k) cfs)
        splitKeys = splitOn ">" ky
        filteredPairs = pairs splitKeys cfgs

filterPairs :: Key -> Pair -> Maybe [Pair]
filterPairs ky (Str k v)      = if ky == k then Just [Str k v] else Nothing
filterPairs ky (Numeric k v)  = if ky == k then Just [Numeric k v] else Nothing
filterPairs ky (Boolean k v)  = if ky == k then Just [Boolean k v] else Nothing
filterPairs ky (Strs k v)     = if ky == k then Just [Strs k v] else Nothing
filterPairs ky (Booleans k v) = if ky == k then Just [Booleans k v] else Nothing
filterPairs ky (Numerics k v) = if ky == k then Just [Numerics k v] else Nothing
filterPairs ky (CObject  k (ConfigPairs prs)) = if ky == k then Just prs else Nothing

getOperator :: Condition -> (Double -> Double -> Bool)
getOperator CGT = (>)
getOperator CLT = (<)
getOperator CLE = (<=)
getOperator CGE = (>=)
getOperator CEQ = (==)
getOperator _   = \_ _ -> False

