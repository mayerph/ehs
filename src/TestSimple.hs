{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSimple where
import Simple
import Language.Haskell.Interpreter

test = ["Wassermelone", "Pfirsich"]
wasser = ["Wassermelone", "Pfirsich"]
a1 = 1
a2 = 3
bla = "2adf"
x = [1,2 :: Int]

-- explicit .. if realisieren?
z = hIf' $! 3

--testSimple = print [simple|Hi[a<-wasser]|]
testSimple = [simple|<z>[a<-wasser]|]

hIf :: a -> Bool
hIf a = True

hIf' :: a -> String
hIf' a = "HI"

getF :: String -> IO (Either InterpreterError (Float -> Float))
getF xs = runInterpreter $ do
   setImports ["Prelude"]
   interpret xs (as :: Float -> Float)