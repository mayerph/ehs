{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Ehs(module Html_Parser, module Html_Data, module Helper, initData) where

import Html_Parser
import Html_Data
import Data
import Helper

initData :: String -> IO()
initData loc = do
    writeFile (loc ++ "Data.hs") templateData


templateData = "{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}\n\
\{-# Language FlexibleContexts #-}\n\
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\{-# LANGUAGE OverloadedStrings #-}\n\
\\n\
\module Data where\n\
\\n\
\import Language.Haskell.TH\n\
\import Language.Haskell.TH.Quote\n\
\import Language.Haskell.TH.Syntax\n\
\import Control.Monad\n\
\import Control.Applicative\n\
\\n\
\data Template = \n\
\\t    Empty\n\
\\t|   T_List [Template]\n\
\\t|   T_String String\n\
\\t|   T_Int Int\n\
\\n\
\\tderiving (Eq, Ord)\n\
\\n\
\instance Lift Template where\n\
\\tlift (T_String i) = appE (conE 'T_String) (lift i)\n\
\\tlift (T_Int i) = appE (conE 'T_Int) (lift i)\n\
\\n\
\instance Show Template where\n\
\\tshow (Empty) = \"Empty\"\n\
\\tshow (T_String i) = id i\n\
\\tshow (T_Int i) = show i\n\
\\tshow (T_List i) = show i"