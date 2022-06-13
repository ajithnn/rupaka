{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Config where

import           Data.Aeson                    hiding (Key, Value)
import           Data.FileEmbed
import           Data.List                     as L
import           Data.Map                      as M
import           Data.Maybe
import           Data.String
import           Data.Text                     as T
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Parsec                   as P
import           Text.ParserCombinators.Parsec
import           THEnv


type Key = String
type Value = String

newtype Config = Config Pair deriving (Show,Generic)
data Pair = Term Key Value | Validation Key Condition Value deriving (Show,Generic)
data Condition = CGT | CLT | CLE | CGE | CEQ | None deriving (Eq,Ord,Generic)
newtype KvPair = KvPair (Key,Value) deriving (Show,Eq,Ord,Generic)

instance ToJSON KvPair where
  toJSON (KvPair (x,y)) = object [ "x" .= x, "y" .= y]

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

configs :: QuasiQuoter
configs = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
          things ++ " are not handled by the regex quasiquoter."

filePath = $(lookupCompileEnvExp "CONFIG_FILEPATH")

compile :: String -> Q Exp
compile s =
  case P.parse configParser "" s of
    Left  err     -> fail (show err)
    Right configs -> [e| configs |]

configParser :: Parsec String () [Config]
configParser = cfgs
  where defineType = many eol >> many space >> string "d"
        values  = many space >> many wordSpaces
        keys  = many space >> many word
        validationType = many eol >> many space >> string "v"
        eol = P.char '\n'
        cfgs = (Config <$> (term <|> validation)) `P.sepBy` eol
        word = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.'])
        wordSpaces = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.',' '])
        conds = oneOf ['>','<','=']
        cond = fromString <$> (many space >> many conds)
        term  = P.try defineType >> Term <$> P.try keys <*> P.try (many space >> P.char ':' *> P.try values)
        validation = P.try validationType >> Validation <$> P.try keys <*> P.try cond <*> P.try values

instance Lift Config where
  lift (Config ps) = appsE [conE 'Config, lift ps]

instance Lift Pair where
  lift (Term k v)         = appsE [conE 'Term, lift k,lift v]
  lift (Validation k c v) = appsE [conE 'Validation, lift k,lift c,lift v]

instance Lift Condition where
  lift CGT  = conE 'CGT
  lift CLT  = conE 'CLT
  lift CLE  = conE 'CLE
  lift CGE  = conE 'CGE
  lift None = conE 'None

configsFromFile :: Maybe FilePath -> Q Exp
configsFromFile (Just rawFp)= do
  fp <- makeRelativeToProject rawFp
  contents <- runIO $ readFile fp
  compile contents
configsFromFile Nothing = compile ""
