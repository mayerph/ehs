{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Ehs(module Html_Parser, module Html_Data, module Helper, initData, renderHtml) where

import Html_Parser
import Html_Data
import Data
import Helper

{-|
  The 'initData' function initialies the basic Template datatype.
-}
initData :: String -> IO()
initData loc = do
    writeFile (loc ++ "Data.hs") templateData

{-|
  The 'renderHtml' function writes the created html to file.
-}
renderHtml :: String -> [HTMLValue] -> IO()
renderHtml file content = do
    writeFile file (list_to_string content)


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
\      Empty\n\
\  |   T_List [Template]\n\
\  |   T_String String\n\
\  |   T_Int Int\n\
\  deriving (Eq, Ord)\n\
\\n\
\instance Lift Template where\n\
\  lift (T_String i) = appE (conE 'T_String) (lift i)\n\
\  lift (T_Int i) = appE (conE 'T_Int) (lift i)\n\
\\n\
\instance Show Template where\n\
\  show (Empty) = \"Empty\"\n\
\  show (T_String i) = id i\n\
\  show (T_Int i) = show i\n\
\  show (T_List i) = show i"