{-# LANGUAGE OverloadedStrings #-}

module Config where
import           Data.FileEmbed
import           Data.List                     as L
import           Data.Map                      as M hiding (keys)
import           Data.Maybe
import           Data.String                   (fromString)
import           Data.Text                     as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec                   as P
import           Text.ParserCombinators.Parsec
import           Text.Read                     as TR
import           Types
import           Util

eol =  P.try (string "\n\r") <|> P.try (string "\n\r") <|> P.try (string "\n") <|> P.try (string "\r") <?> "end of line"
keys  = many space >> many word
word = oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.'])
nums = oneOf ['0'..'9']
bools = P.try (string "True") <|> P.try (string "true") <|> P.try (string "False") <|> P.try (string "false") <?> "boolean value, True or False"

configParser :: Parsec String () [Config]
configParser = manyTill cfgs eof
  where strTerm  = P.try (many space >> string "ds") >> TermS <$> P.try keys <*> P.try (many space >> P.char ':' >> many space *> P.try (many wordSpaces) <* eol)
        intTerm = P.try (many space >> string "di" ) >> TermI <$> P.try keys <*> P.try (many space >> P.char ':' *> (many space >> readNum <$> P.try (many nums) <* eol ))
        boolTerm = P.try (many space >> string "db" ) >> TermB <$> P.try keys <*> P.try (many space >> P.char ':' *> (many space >> readBool <$> P.try bools <* eol ))
        cfgs = Config <$> (strTerm <|> intTerm <|> boolTerm)
        wordSpaces = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.',' ','/',':'])

validationParser :: Parsec String () [Validation]
validationParser = manyTill validations eof
  where validationInt = P.try (many space >> string "vi") >> ValidationI <$> P.try keys <*> P.try cond <*> (many space >> readNum <$>P.try (many nums) <* eol)
        validationStr = P.try (many space >> string "vs") >> ValidationS <$> P.try keys <*> P.try strCond <*> (many space >> P.try strValues <* eol)
        validationBool = P.try (many space >> string "vb") >> ValidationB <$> P.try keys <*> (fromString <$> P.try (many space >> string "is")) <*> (many space >> readBool <$> P.try bools <* eol)
        validations = P.try validationInt <|> P.try validationStr <|> P.try validationBool
        strValues = many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.','[',']','+','*','^','$',':','/','\\','(',')','|']))
        cond = fromString <$> (many space >> many (oneOf ['>','<','=']))
        strCond = fromString <$> (many space >> (string "matches" <|> string "oneof"))

compileConfig :: FilePath -> IO (Either String [Config])
compileConfig fp = do
  s <- readFile fp
  case P.parse configParser "" s of
    Left  err     -> return $ Left (show err)
    Right configs -> return $ Right configs

compileValid :: FilePath -> IO (Either String [Validation])
compileValid fp = do
  s <- readFile fp
  case P.parse validationParser "" s of
    Left  err         -> return $ Left (show err)
    Right validations -> return $ Right validations
