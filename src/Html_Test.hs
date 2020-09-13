{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Html_Test where
import Html_Parser
import Html_Data
import Data
import Helper

a = 1
b = 2

role1 = "student"
role2 = "teacher"

testExpr :: [HTMLValue]
testExpr = [html|<div hIf="T_String role1 != T_String role2 'AND' ('NOT' T_Int a > T_Int b)">Hello World</div>|]

greeting = "Hello"


user1 = Data.User { uid = 1, firstName = "Max", lastName = "Mustermann", income = 2500 }
user2 = Data.User { uid = 2, firstName = "Hans", lastName = "Glueck", income = 3000 }
user3 = Data.User { uid = 3, firstName = "Peter", lastName = "Pan", income = 1500 }
user4 = Data.User { uid = 4, firstName = "Jack", lastName = "Black", income = 3500  }

userGrouped = T_List[T_List[T_User user1, T_User user2], T_List[T_User user3, T_User user4]]

testPlaceholder :: [HTMLValue]
testPlaceholder = [html|<div id="{ T_Int uid user1 }">
    {{ T_String greeting }} World {{ T_String greeting }}, my Name is {{ T_String firstName user1 }} {{ T_String lastName user1 }}</div>|]

testIteration :: [HTMLValue]
testIteration = [html|<div [group <- userGrouped]><p [user <- group] id="{ T_Int uid unpackUser user }">
    Hello {{ T_String firstName unpackUser user }}</p></div>|]


users = T_List[T_User user1, T_User user2, T_User user3, T_User user4]

minIncome = 2500
testAllFeatures :: [HTMLValue]
testAllFeatures = [html|<div [u <- users] id="{ T_Int uid unpackUser u }" hIf="T_Int income unpackUser u > T_Int minIncome">
    My name is {{ T_String firstName unpackUser u }} {{ T_String lastName unpackUser u }}</div>|]


unpackUser :: Template -> User 
unpackUser (T_User a) = a







