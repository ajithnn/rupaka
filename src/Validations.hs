module Validations where

import           Control.Monad
import           Data.Aeson            (encodeFile)
import           Data.ByteString       as B hiding (readFile)
import           Data.ByteString.Char8 as BC hiding (readFile)
import           Data.List             as L (all, concat, elem, foldl, map,
                                             notElem, null, replicate, zip,
                                             (\\))
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
  where resp  | res = Right True
              | otherwise = Left $ mconcat ["Failed - ", show vld]
        res = case vld of
                VStr {}      -> strValidator vld cfgM
                VStrs {}     -> strValidator vld cfgM
                VNumeric {}  -> numValidator vld cfgM
                VNumerics {} -> numValidator vld cfgM
                VKey {}      -> keyValidator vld cfgM
                VKeys {}     -> keyValidator vld cfgM

keyValidator :: Triple -> [Pair] -> Bool
keyValidator vld cfgM = res
  where splitStr = splitOn "|"
        res = case vld of
                (VKey k ALLOWED v)    -> L.all (\vl -> extractKey vl `L.elem` splitStr v) (getPairs cfgM k)
                (VKeys k REQUIRED v)  -> L.all ((\ ks -> L.null $ v L.\\ ks) . extractConfigPairKeys) (getConfigPairs (splitOn ">" k) [ConfigPairs cfgM])
                _ -> False

numValidator :: Triple -> [Pair] -> Bool
numValidator vld cfgM = res
  where res = case vld of
                (VNumeric k c v)  -> L.all (\vl -> getOperator c (extractNum vl) v) (getPairs cfgM k)
                (VNumerics k c v) -> L.all (\vl -> getOperator c vl v) (L.foldl (flip (++)) [] (L.map extractNums (getPairs cfgM k)))
                _ -> False

strValidator :: Triple -> [Pair] -> Bool
strValidator vld cfgM = res
  where splitStr = splitOn "|"
        res =  case vld of
                (VStr k MATCHES v)      -> L.all (\vl -> extractString vl TRT.=~ v :: Bool) (getPairs cfgM k)
                (VStr k NOT_MATCHES v)  -> L.all (\vl -> not (extractString vl TRT.=~ v :: Bool)) (getPairs cfgM k)
                (VStr k ONEOF v)        -> L.all (\vl -> extractString vl `L.elem` splitStr v) (getPairs cfgM k)
                (VStr k NOT_ONEOF v)    -> L.all (\vl -> extractString vl `L.notElem` splitStr v) (getPairs cfgM k)
                (VStr k LENGTH_GT v)    -> checkLength k v cfgM (>)
                (VStr k LENGTH_GTE v)   -> checkLength k v cfgM (>=)
                (VStr k LENGTH_LT v)    -> checkLength k v cfgM (<)
                (VStr k LENGTH_LTE v)   -> checkLength k v cfgM (<=)
                (VStr k LENGTH_EQ v)    -> checkLength k v cfgM (==)
                (VStrs k MATCHES v)     -> checkAllStrings k v cfgM (TRT.=~) id id id
                (VStrs k ONEOF v)       -> checkAllStrings k v cfgM L.elem id splitStr id
                (VStrs k NOT_MATCHES v) -> checkAllStrings k v cfgM (TRT.=~) id id not
                (VStrs k NOT_ONEOF v)   -> checkAllStrings k v cfgM L.elem id splitStr not
                (VStrs k LENGTH_GT v)   -> checkAllStrings k v cfgM (>)  Prelude.length read id
                (VStrs k LENGTH_GTE v)  -> checkAllStrings k v cfgM (>=) Prelude.length read id
                (VStrs k LENGTH_LT v)   -> checkAllStrings k v cfgM (<)  Prelude.length read id
                (VStrs k LENGTH_LTE v)  -> checkAllStrings k v cfgM (<=) Prelude.length read id
                (VStrs k LENGTH_EQ v)   -> checkAllStrings k v cfgM (==) Prelude.length read id
                _ -> False

getPairs :: [Pair] -> Key -> [Pair]
getPairs cfgs ky | ky == "CONFIGROOTKEY" = cfgs
                 | otherwise = filteredPairs
  where pairs [] p        = p
        pairs (k:ks) cfs = pairs ks (L.foldl (flip (++)) [] $ Mb.mapMaybe (filterPairs k) cfs)
        splitKeys = splitOn ">" ky
        filteredPairs = pairs splitKeys cfgs

-- L.all (\ks -> L.null (\ks -> L.null $ (splitOn "|" v) L.\\ ks) (Prelude.map extractConfigPairKeys (getConfigPairs (splitOn ">" key) [ConfigPairs cfgs]))

getConfigPairs :: [Key] -> [ConfigPairs] -> [ConfigPairs]
getConfigPairs [] _ = []
getConfigPairs (k:ks) cpairs  | k == "CONFIGROOTKEY" = cpairs
                              | otherwise = getConfigPairs ks (firstOf $ Prelude.map ((firstOf . Prelude.filter (not . L.null) . Prelude.map (extractObjects k)) . extractConfigPairs) cpairs)
  where firstOf []     = []
        firstOf (v:vs) = v

extractConfigPairs :: ConfigPairs -> [Pair]
extractConfigPairs (ConfigPairs p) = p

extractConfigPairKeys :: ConfigPairs -> [Key]
extractConfigPairKeys (ConfigPairs prs) = Prelude.map extractKey prs

filterPairs :: Key -> Pair -> Maybe [Pair]
filterPairs ky (Str k v)      = if ky == k then Just [Str k v] else Nothing
filterPairs ky (Numeric k v)  = if ky == k then Just [Numeric k v] else Nothing
filterPairs ky (Boolean k v)  = if ky == k then Just [Boolean k v] else Nothing
filterPairs ky (Strs k v)     = if ky == k then Just [Strs k v] else Nothing
filterPairs ky (Booleans k v) = if ky == k then Just [Booleans k v] else Nothing
filterPairs ky (Numerics k v) = if ky == k then Just [Numerics k v] else Nothing
filterPairs ky (CObject  k (ConfigPairs prs)) = if ky == k then Just prs else Nothing
filterPairs ky (CObjects  k []) = if ky == k then Just [] else Nothing
filterPairs ky (CObjects  k (v:vs)) = if ky == k then (++) <$> getPair v <*> filterPairs ky (CObjects k vs) else Nothing
  where getPair (ConfigPairs v) = Just v

getOperator :: Condition -> (Double -> Double -> Bool)
getOperator CGT = (>)
getOperator CLT = (<)
getOperator CLE = (<=)
getOperator CGE = (>=)
getOperator CEQ = (==)
getOperator _   = \_ _ -> False

checkLength k v cfgM f = L.all (\vl -> f ((Prelude.length . extractString) vl) (read v)) (getPairs cfgM k)
checkAllStrings k v cfgM f lf rf negate = L.all (\vl -> negate (f (lf vl) (rf v))) (L.foldl (flip (++)) [] (L.map extractStrings (getPairs cfgM k)))


--ghci> cpairs1 = Prelude.head $ Prelude.map (\cfgM -> Prelude.head $ Prelude.filter (not . L.null) $ Prelude.map (\p -> extractObjects "male" p) cfgM) (Prelude.map extractConfigPairs cpairs)
--ghci> cpairs2 = Prelude.head $ Prelude.map (\cfgM -> Prelude.head $ Prelude.filter (not . L.null) $ Prelude.map (\p -> extractObjects "offices" p) cfgM) (Prelude.map extractConfigPairs cpairs1)
--ghci> cpairs2
--[ConfigPairs ["location" "blr","desg" "Architect"],ConfigPairs ["location" "delhi","desg" "EM","company" "amagi"]]
--ghci> :t cpairs2
--cpairs2 :: [ConfigPairs]
--ghci>
--ghci> Prelude.map extractConfigPairKeys cpairs2
--[["location","desg"],["location","desg","company"]]
--ghci> :t Prelude.map extractConfigPairKeys cpairs2
--Prelude.map extractConfigPairKeys cpairs2 :: [[Key]]
--ghci> Prelude.map extractConfigPairKeys cpairs1
--[["url","run","mode","age","offices"]]
--ghci> :info extractConfigPairs
--extractConfigPairs :: ConfigPairs -> [Pair]
--        -- Defined at <interactive>:61:1
--



