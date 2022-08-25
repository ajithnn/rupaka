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
  where cfgs      = P.try objTerms <|> P.try strTerms <|> P.try intTerms <|> P.try boolTerms <|> P.try objTerm <|> P.try intTerm <|> P.try boolTerm <|> P.try strTerm
        strTerm   = Str      <$> P.try keys <*> P.try (sep >> many space  >> str  <* eol)
        intTerm   = Numeric  <$> P.try keys <*> P.try (sep *> (many space >> num  <* eol ))
        boolTerm  = Boolean  <$> P.try keys <*> P.try (sep *> (many space >> bool <* eol ))
        strTerms  = Strs     <$> P.try keys <*> P.try (sep >> many space  >> strArray <* eol)
        intTerms  = Numerics <$> P.try keys <*> P.try (sep *> (many space >> intArray  <* eol ))
        boolTerms = Booleans <$> P.try keys <*> P.try (sep *> (many space >> boolArray <* eol ))
        objTerms  = CObjects <$> P.try keys <*> P.try (sep *> (many space >>  objArray (many objects)))
        objTerm   = CObject  <$> P.try keys <*> P.try (sep *> (many space >> objects ))
        intArray  = readNums . splitOn "," <$> btwBrackets (many numCommas)
        boolArray = readBools <$> btwBrackets boolCommas
        str =  P.try (many wordSpaces)
        objArray = between (P.try (many space >> P.string "[\n")) (P.try (many space >> P.string "]\n"))
        objects = between (P.try (many space >> P.string "{\n")) (P.try (many space >> P.string "}\n")) (ConfigPairs <$> many cfgs)

-- Validation Parser

validationParser :: Parsec String () VldTriples
validationParser = VldTriples <$> manyTill validations eof
  where validations = P.try validationsInt <|> P.try validationsStr <|> P.try validationKey
        validationKey   = VKey       <$> P.try keys <*> keyCond <*> (many space  >> strArray <* eol)
        validationsInt  = VNumerics  <$> P.try keys <*> cond     <*> (many space >> num <* eol)
        validationsStr  = VStrs      <$> P.try keys <*> strCond  <*> (strValidations <* eol)
        cond            = fromString <$> P.try (many space >> many (oneOf ['>','<','=']))
        strCond         = fromString <$> P.try (many space >> strConditions)
        keyCond         = fromString <$> P.try (many space >> keyConditions)
        boolsArr = bools `P.sepBy` P.char '|'

-- Parser helpers

eol   = P.try (string "\n\r") <|> P.try (string "\n\r") <|> P.try (string "\n") <|> P.try (string "\r") <?> "end of line"
keys  = many space >> many word
nums  = oneOf (['0'..'9']++['.'])
num   = readNum  <$> P.try (many nums)
bool  = readBool <$> P.try bools
sep   = many space >> P.char ':'
word  = oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.','>']) <?> "valid word"
bools = P.try (string "True") <|> P.try (string "true") <|> P.try (string "False") <|> P.try (string "false") <?> "boolean value"
regex = ['.', '+', '*', '?', '^', '$', '(', ')', '[', ']', '{', '}', '|', '\\','<','>']
strValidations = many space >> P.try (many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.','[',']',':','/','!']++regex))) <?> "valid string"
strArray  = splitOn "," <$> btwBrackets (many wordSpaces) 
wordSpaces = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.',' ','/',':',';',',','?','{','}','!','@','#','$','%','^','&','*','(',')','+','=']) <?> "valid words"
numCommas     = oneOf (['0'..'9']++['.',',']) <?> "number or array of numbers"
boolCommas    = bools `P.sepBy` P.char ','
keyConditions = P.try (string "allowed") <|> P.try (string "required") <?> "valid key condition allowed|required"
btwBrackets vals  = P.try (between (P.char '[') (P.char ']') vals)
strConditions     = P.try (string "not_matches")  <|>
                    P.try (string "not_oneof")    <|>
                    P.try (string "length_gt")    <|>
                    P.try (string "length_lt")    <|>
                    P.try (string "length_gte")   <|>
                    P.try (string "length_lte")   <|>
                    P.try (string "length_eq")    <|>
                    P.try (string "matches")      <|>
                    P.try (string "oneof")        <?> 
                    "valid conditions not_matches|not_oneof|length_gt|length_lt|length_gte|length_lte|length_eq|matches|oneof"
