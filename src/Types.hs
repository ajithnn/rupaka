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
data Pair = Str  Key String       |
            Strs Key [String]     |
            Numeric  Key Double   |
            Numerics Key [Double] |
            Booleans Key [Bool]   |
            Boolean  Key Bool     |
            CObject  Key ConfigPairs deriving (Generic)

newtype VldTriples = VldTriples [Triple] deriving (Show,Generic)
data Triple = VStr Key Condition String      |
              VStrs Key Condition String     |
              VNumeric Key Condition Double  |
              VNumerics Key Condition Double |
              VBoolean Key Condition Bool    |
              VBooleans Key Condition [Bool] deriving (Generic)

data Condition =  CGT |
                  CLT |
                  CLE |
                  CGE |
                  CEQ |
                  MATCHES |
                  IS |
                  ONEOF |
                  None deriving (Eq,Ord,Generic)

data InputOptions = InputOptions{
  configpath     :: FilePath,
  validationPath :: FilePath,
  outputPath     :: FilePath
}

instance Show Pair where
  show (Numeric k v)  = mconcat [show k, " ", show v]
  show (Numerics k v) = mconcat [show k, " ", show v]
  show (Str k v)      = mconcat [show k, " ", show v]
  show (Strs k v)     = mconcat [show k, " ", show v]
  show (Boolean k v)  = mconcat [show k, " ", show v]
  show (Booleans k v) = mconcat [show k, " ", show v]
  show (CObject k v)  = mconcat [show k, " ", show v]

instance Show Triple where
  show (VNumeric k c v)  = mconcat [show k, " ", show c, " ", show v]
  show (VNumerics k c v) = mconcat [show k, " ", show c, " ", show v]
  show (VStr k c v)      = mconcat [show k, " ", show c, " ", show v]
  show (VStrs k c v)     = mconcat [show k, " ", show c, " ", show v]
  show (VBoolean k c v)  = mconcat [show k, " ", show c, " ", show v]
  show (VBooleans k c v) = mconcat [show k, " ", show c, " ", show v]

instance IsString Condition where
  fromString c = case c of
    ">"       -> CGT
    "<"       -> CLT
    ">="      -> CGE
    "<="      -> CLE
    "=="      -> CEQ
    "matches" -> MATCHES
    "is"      -> IS
    "oneof"   -> ONEOF
    _         -> None

instance Show Condition where
  show CGT     = ">"
  show CLT     = "<"
  show CGE     = ">="
  show CLE     = "<="
  show CEQ     = "=="
  show MATCHES = "matches"
  show IS      = "is"
  show ONEOF   = "oneof"
  show None    = ""

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
