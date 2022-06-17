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

newtype Config = Config Pair deriving (Show,Generic)
data Pair = TermS Key ValueS | TermI Key ValueI deriving (Show,Generic)
data Validation = ValidationS Key Condition ValueS | ValidationI Key Condition ValueI deriving (Show,Generic)
data Condition = CGT | CLT | CLE | CGE | CEQ | MATCHES | None deriving (Eq,Ord,Generic)

data InputOptions = InputOptions{
  configpath     :: FilePath,
  validationPath :: FilePath,
  outputPath     :: FilePath
}

instance IsString Condition where
  fromString c = case c of
    ">"       -> CGT
    "<"       -> CLT
    ">="      -> CGE
    "<="      -> CLE
    "=="      -> CEQ
    "matches" -> MATCHES
    _         -> None

instance Show Condition where
  show CGT     = ">"
  show CLT     = "<"
  show CGE     = ">="
  show CLE     = "<="
  show CEQ     = "=="
  show MATCHES = "matches"
  show None    = ""

instance ToJSON Config where
instance ToJSON Pair where
instance ToJSON Condition where
