module Validations where

import           Control.Monad
import           Data.Aeson            (encodeFile)
import           Data.ByteString       as B hiding (readFile)
import           Data.ByteString.Char8 as BC hiding (readFile)
import           Data.List             as L (all, concat, elem, foldl, map,
                                             replicate, zip)
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
                (VStr  k MATCHES v)   -> L.all (\vl -> extractString vl TRT.=~ v :: Bool) (getPairs cfgM k)
                (VBoolean k IS v)     -> L.all (\vl -> extractBool vl == v) (getPairs cfgM k)
                (VNumeric k c v)      -> L.all (\vl -> getOperator c (extractNum vl) v) (getPairs cfgM k)
                (VStr  k ONEOF v)     -> L.all (\vl -> extractString vl `L.elem` splitOn "|" v) (getPairs cfgM k)
                (VStrs k ONEOF v)     -> L.all (\vl -> vl `L.elem` splitStr v) (L.foldl (flip (++)) [] (L.map extractStrings (getPairs cfgM k)))
                (VNumerics k c v)     -> L.all (\vl -> getOperator c vl v) (L.foldl (flip (++)) [] (L.map extractNums (getPairs cfgM k)))
                (VBooleans k ONEOF v) -> L.all (`L.elem` v) (L.foldl (flip (++)) [] (L.map extractBools (getPairs cfgM k)))
                (VStrs k MATCHES v)   -> L.all (\vl -> vl TRT.=~ v :: Bool) (L.foldl (flip (++)) [] (L.map extractStrings (getPairs cfgM k)))
                _ -> False
        splitStr = splitOn "|"
        resp  | res = Right True
              | otherwise = Left $ mconcat ["Failed - ", show vld]

getPairs :: [Pair] -> Key -> [Pair]
getPairs cfgs ky = filteredPairs
  where firstOf []     = Nothing
        firstOf (x:xs) = Just x
        pairs [] p        = p
        pairs (k:ks) cfs = pairs ks (L.foldl (flip (++)) [] $ Mb.mapMaybe (filterPairs k) cfs)
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
filterPairs ky (CObjects  k (v:vs)) = if ky == k then (++) <$> getPair v <*> filterPairs ky (CObjects k vs) else Nothing
filterPairs ky (CObjects  k []) = if ky == k then Just [] else Nothing

getPair :: ConfigPairs -> Maybe [Pair]
getPair (ConfigPairs v) = Just v

getOperator :: Condition -> (Double -> Double -> Bool)
getOperator CGT = (>)
getOperator CLT = (<)
getOperator CLE = (<=)
getOperator CGE = (>=)
getOperator CEQ = (==)
getOperator _   = \_ _ -> False

