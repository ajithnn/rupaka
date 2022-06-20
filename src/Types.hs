{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson                 hiding (Key, Value)
import           Data.Maybe
import           Data.String
import           GHC.Generics
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

type Key = String
type ValueS = String
type ValueI = Int
type ValueB = Bool

newtype Config = Config Pair deriving (Show,Generic)

newtype Output = Output [Config] deriving (Show,Generic)

data Pair = TermS Key ValueS |
            TermI Key ValueI |
            TermB Key ValueB deriving (Show,Generic)

data Validation = ValidationS Key Condition ValueS |
                  ValidationI Key Condition ValueI |
                  ValidationB Key Condition ValueB deriving (Generic)

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
  show (ValidationI k c v) = mconcat [show k, " ", show c, " ", show v]
  show (ValidationS k c v) = mconcat [k, " ", show c, " ",v]
  show (ValidationB k c v) = mconcat [k, " ", show c, " ",show v]

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

instance ToJSON Output where
  toJSON (Output cfgs) = object $ final cfgs
    where final (c:cs) = val c : final cs
          final _      = []
          val (Config (TermS k v)) = fromString k .= v
          val (Config (TermB k v)) = fromString k .= v
          val (Config (TermI k v)) = fromString k .= v

instance ToJSON Config where
instance ToJSON Pair where
instance ToJSON Condition where
