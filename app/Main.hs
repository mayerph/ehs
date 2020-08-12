{-# LANGUAGE InstanceSigs #-}
module Main where

import Lib
import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad



main :: IO ()
main = do
    x <- parseFromFile jsonValue "data.txt"
    print x
