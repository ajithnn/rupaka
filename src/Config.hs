{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config where
import           Data.FileEmbed
import           Data.List                     as L
import           Data.Map                      as M
import           Data.String                   (fromString)
import           Data.Text                     as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec                   as P
import           Text.ParserCombinators.Parsec
import           THEnv
import           Types

valid :: QuasiQuoter
valid = QuasiQuoter {
    quoteExp  = compileValid
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
          things ++ " are not handled by the regex quasiquoter."

filePath = $(lookupCompileEnvExp "CONFIG_FILEPATH")

compileValid :: String -> Q Exp
compileValid s =
  case P.parse validationParser "" s of
    Left  err         -> fail (show err)
    Right validations -> [e| validations |]

configParser :: Parsec String () [Config]
configParser = cfgs
  where defineType = many eol >> many space >> string "d"
        values  = many space >> many wordSpaces
        keys  = many space >> many word
        eol = P.char '\n'
        cfgs = (Config <$> term) `P.sepBy` eol
        word = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.'])
        wordSpaces = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.',' '])
        term  = P.try defineType >> Term <$> P.try keys <*> P.try (many space >> P.char ':' *> P.try values)

validationParser :: Parsec String () [Validation]
validationParser = validations
    where validation = P.try validationType >> Validation <$> P.try keys <*> P.try cond <*> P.try values
          validations = validation `P.sepBy` eol
          validationType = many eol >> many space >> string "v"
          eol = P.char '\n'
          keys  = many space >> many word
          values  = many space >> many wordSpaces
          wordSpaces = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.',' '])
          word = oneOf(['a'..'z']++['A'..'Z']++['0'..'9']++['_','-','.'])
          conds = oneOf ['>','<','=']
          cond = fromString <$> (many space >> many conds)

validationsFromFile :: Maybe FilePath -> Q Exp
validationsFromFile (Just rawFp)= do
  fp <- makeRelativeToProject rawFp
  contents <- runIO $ readFile fp
  compileValid contents
validationsFromFile Nothing = compileValid ""

compileConfig :: FilePath -> IO (Either String [Config])
compileConfig fp = do
  s <- readFile fp
  case P.parse configParser "" s of
    Left  err     -> return $ Left (show err)
    Right configs -> return $ Right configs
