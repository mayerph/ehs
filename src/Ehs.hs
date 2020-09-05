{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Ehs(module Html_Parser, module Html_Data, module Helper) where

import Html_Parser
import Html_Data
import Data
import Helper

initData :: String -> IO()
initData loc = do
    writeFile (loc ++ "Data.hs") templateData


templateData = "{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}\
\{-# Language FlexibleContexts #-}\
\{-# LANGUAGE NoMonomorphismRestriction #-}\
\{-# LANGUAGE OverloadedStrings #-}\
\\
\module Data where \
\\
\import Language.Haskell.TH \
\import Language.Haskell.TH.Quote\
\import Language.Haskell.TH.Syntax \
\import Control.Monad\
\import Control.Applicative\
\\
\data Template = \
        \Empty\
    \|   T_List [Template]\
    \|   T_String String \
    \|   T_Int Int \
\ \
    \deriving (Eq, Ord)\
\\
\instance Lift Template where\
    \lift (T_String i) = appE (conE 'T_String) (lift i)\
    \lift (T_Int i) = appE (conE 'T_Int) (lift i)\
\\
\instance Show Template where\
    \show (Empty) = \"Empty\"\
    \show (T_String i) = id i\
    \show (T_Int i) = show i\
    \show (T_List i) = show i"