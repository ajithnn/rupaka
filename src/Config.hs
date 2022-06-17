{-# LANGUAGE OverloadedStrings #-}

module Config where
import           Data.FileEmbed
import           Data.List                     as L
import           Data.Map                      as M
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

configParser :: Parsec String () [Config]
configParser = manyTill cfgs eof
  where strTerm  = P.try defineTypeStr >> TermS <$> P.try keys <*> P.try (many space >> P.char ':' *> P.try values <* eol)
        intTerm = P.try defineTypeInt >> TermI <$> P.try keys <*> P.try (many space >> P.char ':' *> (many space >> readNum <$> P.try (many nums) <* eol ))
        defineTypeStr = many space >> string "ds"
        defineTypeInt = many space >> string "di"
        values  = many space >> many wordSpaces
        keys  = many space >> many word
        eol =  P.try (string "\n\r") <|> P.try (string "\n\r") <|> P.try (string "\n") <|> P.try (string "\r") <?> "end of line"
        cfgs = Config <$> (strTerm <|> intTerm)
        word = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.'])
        wordSpaces = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.',' '])
        nums = oneOf ['0'..'9']

validationParser :: Parsec String () [Validation]
validationParser = manyTill validations eof
  where validationInt = P.try validationTypeI >> ValidationI <$> P.try keys <*> P.try cond <*> (many space >> readNum <$>P.try values <* eol)
        validationStr = P.try validationTypeS >> ValidationS <$> P.try keys <*> P.try strCond <*> (many space >> P.try strValues <* eol)
        validations = P.try validationInt <|> P.try validationStr
        validationTypeI = many space >> string "vi"
        validationTypeS = many space >> string "vs"
        eol =  P.try (string "\n\r") <|> P.try (string "\n\r") <|> P.try (string "\n") <|> P.try (string "\r") <?> "end of line"
        keys  = many space >> many word
        values  = many nums
        strValues = many word
        nums = oneOf ['0'..'9']
        word = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.','[',']','+','*','^','$'])
        conds = oneOf ['>','<','=']
        strConds = many space >> string "matches"
        cond = fromString <$> (many space >> many conds)
        strCond = fromString <$> strConds

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



