
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Template where
import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad



data Format = L String -- ^ literal string, z. B hey
        | S -- ^ %s -> muss ein String sein. 
        | G -- ^ %g generic type -> kann auch Boolean sein, muss aber von 
    deriving Show


-- lamE { \ p1 p2 -> e } 
-- lamE args . appE conc $ listE es
--      -p1-   ---------p2--------  

-- appE { f x } 
-- appE conc $ listE es
--      -f-     ---x---   

printfP :: [Format] -> ExpQ
printfP fmts = lamE args . appE conc $ listE es
                where   (nvars,es) = mapAccumL toExpQ 0 fmts
                        args = map varPat [0 .. (nvars-1)]
                        conc = [|concat|]

-- appE (varE ’show) <--------ist das gleich wie appE--------> [|show|]
--

printf :: String -> ExpQ
printf = printfP . format

string :: String -> ExpQ -- ^ quote the string
string = litE . StringL

-- varE = 	{ x }
varExp :: Int -> ExpQ -- ^ quoted variable xi
varExp i = unboundVarE $ mkName ("x" ++ show i)

-- varP = 	{ x }
varPat :: Int -> PatQ -- ^ quoted pattern xi
varPat i = varP $ mkName ("x" ++ show i)




showE :: ExpQ -> ExpQ -- ^ quoted showing
showE = appE [|show|]

toExpQ :: Int -> Format -> (Int,ExpQ) 
toExpQ i (L s) = (i,string s)
toExpQ i S = (i+1,varExp i)
toExpQ i G = (i+1,showE $ varExp i)

-- parser
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