{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module HelperData where 

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad
import Control.Applicative

data Wrapper = 
        Empty
    |   StringWrapper String 
    |   IntegerWrapper Int 
    |   Wrapper_L [Wrapper]
    deriving (Show, Eq, Ord)

instance Lift Wrapper where
    lift (StringWrapper i) = appE (conE 'StringWrapper) (lift i)
    lift (IntegerWrapper i) = appE (conE 'IntegerWrapper) (lift i)