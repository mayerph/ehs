{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Expression4 where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

data Expr = Term Term
          | Apply Expr Expr
          | If Expr Expr Expr
          deriving (Show)

data Term = Bool Bool
          | Num Double
          | String String
          | Identifier String
          | Parens Expr
          deriving (Show)

keywords = ["if", "then", "else", "True", "False"]
lexeme p = p <* spaces
schar = lexeme . char

keyword k = lexeme . try $
  string k <* notFollowedBy alphaNum

pBool :: Parser Bool
pBool = (True <$ keyword "True") <|> (False <$ keyword "False")

pDouble :: Parser Double
pDouble = lexeme $ do
  ds <- many1 digit
  option (read ds) $ do
    char '.'
    ds' <- many1 digit
    return $ read (ds ++ "." ++ ds')

pString :: Parser String
pString = lexeme . between (char '"') (char '"') . many1 $ noneOf "\""

pIdentifier :: Parser String
pIdentifier = lexeme . try $ do
  ident <- many1 letter
  if ident `elem` keywords
    then unexpected $ "reserved word " ++ show ident
    else return ident

pParens :: Parser Expr
pParens = between (schar '(') (schar ')') pExpr

pTerm :: Parser Term
pTerm = choice [ Bool       <$> pBool
               , Num        <$> pDouble
               , String     <$> pString
               , Identifier <$> pIdentifier
               , Parens     <$> pParens
               ]

-- TODO: make this left-associative
pApply :: Parser Expr
pApply = do
  term <- pTerm'
  option term $
    Apply term <$> pApply

-- pulls "parens" expressions out of terms
pTerm' :: Parser Expr
pTerm' = do
  term <- pTerm
  case term of
    Parens expr -> return expr
    _ -> return $ Term term

pIf :: Parser Expr
pIf = If <$ keyword "if"   <*> pExpr 
         <* keyword "then" <*> pExpr
         <* keyword "else" <*> pExpr

pExpr :: Parser Expr
pExpr = pIf <|> pApply

testwas parser = parse (spaces *> parser <* eof) ""