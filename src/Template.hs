
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Template where
import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad



data Format = L String -- ^ literal string
        | S -- ^ %s
        | G -- ^ %g generic type
    deriving Show

printfP :: [Format] -> ExpQ
printfP fmts = lamE args . appE conc $ listE es
                where   (nvars,es) = mapAccumL toExpQ 0 fmts
                        args = map varPat [0 .. (nvars-1)]
                        conc = [|concat|]

printf :: String -> ExpQ
printf = printfP . format

string = litE . StringL
varExp i = varE $ mkName ("x" ++ show i)
varPat i = varP $ mkName ("x" ++ show i)
showE = appE [|show|]

toExpQ :: Int -> Format -> (Int,ExpQ) 
toExpQ i (L s) = (i,string s)
toExpQ i S = (i+1,varExp i)
toExpQ i G = (i+1,showE $ varExp i)

format :: String -> [Format]
format "" = []
format ['%'] = [L "%"]
format ('%':x:xs)
    | x == 's' = S : format xs
    | x == 'g' = G : format xs
    | x == '%' = L "%" : format xs
    | otherwise = L ['%',x] : format xs
format zs = L x : format xs
    where (x,xs) = span (/='%') zs