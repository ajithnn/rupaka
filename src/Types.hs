{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson                 hiding (Key, Value)
import           Data.Maybe
import           Data.String
import           GHC.Generics
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

type Key = String
type Value = String

newtype Config = Config Pair deriving (Show,Generic)
data Pair = Term Key Value deriving (Show,Generic)
data Validation = Validation Key Condition Value deriving (Show,Generic)
data Condition = CGT | CLT | CLE | CGE | CEQ | None deriving (Eq,Ord,Generic)
newtype KvPair = KvPair (Key,Value) deriving (Show,Eq,Ord,Generic)

data InputOptions = InputOptions{
  configpath     :: FilePath,
  validationPath :: FilePath,
  outputPath     :: FilePath
}

instance IsString Condition where
  fromString c = case c of
    ">"  -> CGT
    "<"  -> CLT
    ">=" -> CGE
    "<=" -> CLE
    "==" -> CEQ
    _    -> None

instance Show Condition where
  show CGT  = ">"
  show CLT  = "<"
  show CGE  = ">="
  show CLE  = "<="
  show CEQ  = "=="
  show None = ""

instance ToJSON Config where
instance ToJSON Pair where
instance ToJSON Condition where
