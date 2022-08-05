{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson                 hiding (Key, Value)
import           Data.List                  as L (isInfixOf)
import           Data.List.Split
import           Data.Maybe
import           Data.String
import           GHC.Generics
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

type Key = String

newtype ConfigPairs = ConfigPairs [Pair] deriving (Show,Generic)
data Pair = Str  Key String             |
            Strs Key [String]           |
            Numeric  Key Double         |
            Numerics Key [Double]       |
            Booleans Key [Bool]         |
            Boolean  Key Bool           |
            CObjects Key [ConfigPairs]  |
            CObject  Key ConfigPairs deriving (Generic)

newtype VldTriples = VldTriples [Triple] deriving (Show,Generic)
data Triple = VStr Key Condition String      |
              VStrs Key Condition String     |
              VNumeric Key Condition Double  |
              VNumerics Key Condition Double |
              VKey Key Condition [String] deriving (Generic)

data Condition =  CGT |
                  CLT |
                  CLE |
                  CGE |
                  CEQ |
                  MATCHES   |
                  REQUIRED  |
                  ALLOWED   |
                  ONEOF     |
                  NOT_MATCHES |
                  NOT_ONEOF   |
                  LENGTH_GT   |
                  LENGTH_GTE  |
                  LENGTH_LT   |
                  LENGTH_LTE  |
                  LENGTH_EQ   |
                  None deriving (Eq,Ord,Generic)

data InputOptions = InputOptions{
  configpath     :: FilePath,
  validationPath :: FilePath,
  outputPath     :: FilePath,
  silent         :: Bool
}

instance Show Pair where
  show (Numeric k v)  = mconcat [show k, " ", show v]
  show (Numerics k v) = mconcat [show k, " ", show v]
  show (Str k v)      = mconcat [show k, " ", show v]
  show (Strs k v)     = mconcat [show k, " ", show v]
  show (Boolean k v)  = mconcat [show k, " ", show v]
  show (Booleans k v) = mconcat [show k, " ", show v]
  show (CObject k v)  = mconcat [show k, " ", show v]
  show (CObjects k v) = mconcat [show k, " ", show v]

instance Show Triple where
  show (VNumeric k c v)  = mconcat [show k, " ", show c, " ", show v]
  show (VNumerics k c v) = mconcat [show k, " ", show c, " ", show v]
  show (VStr k c v)      = mconcat [show k, " ", show c, " ", show v]
  show (VStrs k c v)     = mconcat [show k, " ", show c, " ", show v]
  show (VKey k c v)      = mconcat [show k, " ", show c, " ", show v]


instance IsString Condition where
  fromString c = case c of
    ">"           -> CGT
    "<"           -> CLT
    ">="          -> CGE
    "<="          -> CLE
    "=="          -> CEQ
    "matches"     -> MATCHES
    "oneof"       -> ONEOF
    "required"    -> REQUIRED
    "allowed"     -> ALLOWED
    "not_matches" -> NOT_MATCHES
    "not_oneof"   -> NOT_ONEOF
    "length_gt"   -> LENGTH_GT
    "length_gte"  -> LENGTH_GTE
    "length_lt"   -> LENGTH_LT
    "length_lte"  -> LENGTH_LTE
    "length_eq"   -> LENGTH_EQ
    _             -> None

instance Show Condition where
  show CGT         = ">"
  show CLT         = "<"
  show CGE         = ">="
  show CLE         = "<="
  show CEQ         = "=="
  show MATCHES     = "should match"
  show REQUIRED    = "needs required keys"
  show ALLOWED     = "should have only allowed keys"
  show ONEOF       = "should be one of"
  show NOT_MATCHES = "should not match"
  show NOT_ONEOF   = "should not be one of"
  show LENGTH_GT   = "length >"
  show LENGTH_GTE  = "length >="
  show LENGTH_LT   = "length <"
  show LENGTH_LTE  = "length <="
  show LENGTH_EQ   = "length =="
  show None        = ""

instance ToJSON Condition where

instance ToJSON ConfigPairs where
  toJSON (ConfigPairs cfgs) = object $ final cfgs
    where final (c:cs) = val c : final cs
          final _      = []
          val (Str k v)      = fromString k .= v
          val (Strs k v)     = fromString k .= v
          val (Boolean k v)  = fromString k .= v
          val (Numeric k v)  = fromString k .= v
          val (Booleans k v) = fromString k .= v
          val (Numerics k v) = fromString k .= v
          val (CObject k v)  = fromString k .= v
          val (CObjects k v) = fromString k .= v
