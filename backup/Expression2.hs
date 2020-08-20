{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Expression2 where


import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

import Data.Functor.Identity
import Data.Maybe
import Data.Functor

data Expr
  = Tr
  | Fl
  | Zero
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  deriving (Eq, Show)

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
    { Tok.commentStart    = "{-"
    , Tok.commentEnd      = "-}"
    , Tok.commentLine     = "--"
    , Tok.nestedComments  = True
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames   = []
    , Tok.reservedOpNames = []
    , Tok.caseSensitive   = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)


-- Prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = [
    [
        prefixOp "succ" Succ
    , prefixOp "pred" Pred
    , prefixOp "iszero" IsZero
    ]
    ]


-- if/then/else
ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond <- expr
    reservedOp "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return (If cond tr fl)

-- Constants
true, false, zero :: Parser Expr
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl
zero  = reservedOp "0"   >> return Zero

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
        true
    <|> false
    <|> zero
    <|> ifthen
    <|> parens expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

testIt :: String -> IO ()
testIt a =  do
    let res = parseExpr a
    case res of
        Left err -> print err
        Right ex -> case eval ex of
            Nothing -> putStrLn "Cannot evaluate"
            Just result -> putStrLn $ ppexpr result

isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval' :: Expr -> Maybe Expr
eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval' t)
  Succ t                    -> Succ <$> (eval' t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval' t)
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval' t
  _                         -> Nothing

nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"

parensIf ::  Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Expr where
    ppr _ Zero = PP.text "0"
    ppr _ Tr = PP.text "true"
    ppr _ Fl = PP.text "false"
    ppr p (Succ a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
    ppr p (Pred a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
    ppr p (IsZero a) = (parensIf (p > 0) $ PP.text "iszero" <+> ppr (p+1) a)
    ppr p (If a b c) =
            PP.text "if"   <+> ppr p a
        <+> PP.text "then" <+> ppr p b
        <+> PP.text "else" <+> ppr p c

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0