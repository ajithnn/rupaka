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
type ValueS = String
type ValueSA = [String]
type ValueNA = [Double]
type ValueBA = [Bool]
type ValueN = Double
type ValueB = Bool


newtype Config = Config Pair deriving (Show,Generic)
newtype Configs = Configs [Config] deriving (Show,Generic)

data Values = ValueS | ValueN | ValueB | ValueSA

data Pair = TermS  Key ValueS  |
            TermSA Key ValueSA |
            TermI  Key ValueN  |
            TermIA Key ValueNA |
            TermBA Key ValueBA |
            TermB  Key ValueB deriving (Show,Generic)

data Validation = ValidationS  Key Condition ValueS  |
                  ValidationI  Key Condition ValueN  |
                  ValidationB  Key Condition ValueB  |
                  ValidationSA Key Condition ValueS  |
                  ValidationIA Key Condition ValueN  |
                  ValidationBA Key Condition ValueBA deriving (Generic)

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

instance Show Validation where
  show (ValidationI k c v)  = mconcat [show k, " ", show c, " ", show v]
  show (ValidationS k c v)  = mconcat [k, " ", show c, " ",v]
  show (ValidationB k c v)  = mconcat [k, " ", show c, " ",show v]
  show (ValidationBA k c v) = mconcat [k, " ", show c, " ",show v]
  show (ValidationSA k c v) = mconcat [k, " ", show c, " ",show v]
  show (ValidationIA k c v) = mconcat [k, " ", show c, " ",show v]

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

instance ToJSON Configs where
  toJSON (Configs cfgs) = object $ final cfgs
    where final (c:cs) = val c : final cs
          final _      = []
          val (Config (TermS k v))  = fromString k .= v
          val (Config (TermSA k v)) = fromString k .= v
          val (Config (TermB k v))  = fromString k .= v
          val (Config (TermI k v))  = fromString k .= v
          val (Config (TermBA k v)) = fromString k .= v
          val (Config (TermIA k v)) = fromString k .= v


instance ToJSON Config where
instance ToJSON Pair where
instance ToJSON Condition where
