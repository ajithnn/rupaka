{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where
import           Data.List                     as L
import           Data.List.Split               (splitOn)
import           Data.Map                      as M hiding (keys)
import           Data.Maybe
import           Data.String                   (fromString)
import           Data.Text                     as T hiding (splitOn)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec                   as P
import           Text.ParserCombinators.Parsec
import           Text.Read                     as TR
import           Types
import           Util


-- Call Config Parser

compileConfig :: FilePath -> IO (Either String ConfigPairs)
compileConfig fp = do
  s <- readFile fp
  case P.parse configParser "" s of
    Left  err     -> return $ Left (show err)
    Right configs -> return $ Right configs

-- Call Validation Parser

compileValid :: FilePath -> IO (Either String VldTriples)
compileValid fp = do
  s <- readFile fp
  case P.parse validationParser "" s of
    Left  err         -> return $ Left (show err)
    Right validations -> return $ Right validations

-- Config Parser

configParser :: Parsec String () ConfigPairs
configParser = ConfigPairs <$> manyTill cfgs eof
  where cfgs      = objTerms <|> strTerms <|> intTerms <|> boolTerms <|> objTerm <|> intTerm <|> boolTerm <|> strTerm
        strTerm   = typeTerm "str"    >> Str      <$> P.try keys <*> P.try (sep >> many space  >> str  <* eol)
        intTerm   = typeTerm "num"    >> Numeric  <$> P.try keys <*> P.try (sep *> (many space >> num  <* eol ))
        boolTerm  = typeTerm "bool"   >> Boolean  <$> P.try keys <*> P.try (sep *> (many space >> bool <* eol ))
        strTerms  = typeTerm "[str]"  >> Strs     <$> P.try keys <*> P.try (sep >> many space  >> strArray <* eol)
        intTerms  = typeTerm "[num]"  >> Numerics <$> P.try keys <*> P.try (sep *> (many space >> intArray  <* eol ))
        boolTerms = typeTerm "[bool]" >> Booleans <$> P.try keys <*> P.try (sep *> (many space >> boolArray <* eol ))
        objTerms = typeTerm "[obj]"  >> CObjects <$> P.try keys <*> P.try (sep *> (many space >> between (P.string "[\n") (P.string "]\n") (many objects) ))
        objTerm  = typeTerm "obj"   >> CObject  <$> P.try keys <*> P.try (sep *> (many space >> objects ))
        strArray  = splitOn "," <$> btwBrackets (many wordSpaces)
        intArray  = readNums . splitOn "," <$> btwBrackets (many numCommas)
        boolArray = readBools <$> btwBrackets boolCommas
        str =  P.try (many wordSpaces)
        wordSpaces = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.',' ','/',':',','])
        objects = between (P.string "{\n") (P.string "}\n") (ConfigPairs <$> many cfgs)

-- Validation Parser

validationParser :: Parsec String () VldTriples
validationParser = VldTriples <$> manyTill validations eof
  where validations = validationsInt <|> validationsStr <|> validationsBool <|> validationInt <|> validationStr <|> validationBool
        validationInt   = typeTerm "num"    >> VNumeric   <$> P.try keys <*> cond     <*> (many space >> num <* eol)
        validationStr   = typeTerm "str"    >> VStr       <$> P.try keys <*> strCond  <*> (strValidations <* eol)
        validationBool  = typeTerm "bool"   >> VBoolean   <$> P.try keys <*> isCond   <*> (many space >> bool <* eol)
        validationsInt  = typeTerm "[num]"  >> VNumerics  <$> P.try keys <*> cond     <*> (many space >> num <* eol)
        validationsStr  = typeTerm "[str]"  >> VStrs      <$> P.try keys <*> strCond  <*> (strValidations <* eol)
        validationsBool = typeTerm "[bool]" >> VBooleans  <$> P.try keys <*> boolCond <*> (many space >> readBools <$> P.try boolsArr <* eol)
        cond =      fromString <$> P.try (many space >> many (oneOf ['>','<','=']))
        strCond =   fromString <$> P.try (many space >> (string "matches" <|> string "oneof"))
        boolCond =  fromString <$> P.try (many space >> (string "is" <|> string "oneof"))
        isCond =    fromString <$> P.try (many space >> string "is")
        boolsArr = bools `P.sepBy` P.char '|'

-- Parser helpers

eol =  P.try (string "\n\r") <|> P.try (string "\n\r") <|> P.try (string "\n") <|> P.try (string "\r") <?> "end of line"
keys  = many space >> many word
word = oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.','>'])
nums = oneOf (['0'..'9']++['.'])
num = readNum <$> P.try (many nums)
bools = P.try (string "True") <|> P.try (string "true") <|> P.try (string "False") <|> P.try (string "false") <?> "boolean value, True or False"
bool = readBool <$> P.try bools
typeTerm typ = P.try (many space >> string typ)
sep = many space >> P.char ':'
numCommas = oneOf (['0'..'9']++['.',','])
boolCommas = bools `P.sepBy` P.char ','
btwBrackets vals = P.try (between (P.char '[') (P.char ']') vals)
strValidations = many space >> P.try (many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.','[',']','+','*','^','$',':','/','\\','(',')','|'])))