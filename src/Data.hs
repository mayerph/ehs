{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Data where 

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad
import Control.Applicative

data User = User {uid :: Int, firstName :: String, lastName :: String, income :: Int} 
    deriving (Show, Eq, Ord)

data Template = 
        Empty
    |   T_List [Template]
    |   T_String String 
    |   T_Int Int 
    |   T_User User

    
    deriving (Eq, Ord)

instance Lift Template where
    lift (T_String i) = appE (conE 'T_String) (lift i)
    lift (T_Int i) = appE (conE 'T_Int) (lift i)
    lift (T_List i) = appE (conE 'T_List) (lift i)


instance Show Template where
    show (Empty) = "Empty"
    show (T_String i) = id i
    show (T_Int i) = show i
    show (T_List i) = show i
    show (T_User i) = show i

