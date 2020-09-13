{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Html_Test where
import Html_Parser
import Html_Data
import Data
import Helper
import Ehs

a = 1
b = 2

role1 = "student"
role2 = "teacher"
greeting = "Hello"

user1 = Data.User { uid = 1, firstName = "Max", lastName = "Mustermann", income = 2500 }
user2 = Data.User { uid = 2, firstName = "Hans", lastName = "Glueck", income = 3000 }
user3 = Data.User { uid = 3, firstName = "Peter", lastName = "Pan", income = 1500 }
user4 = Data.User { uid = 4, firstName = "Jack", lastName = "Black", income = 3500  }

userGrouped = T_List[T_List[T_User user1, T_User user2], T_List[T_User user3, T_User user4]]
users = T_List[T_User user1, T_User user2, T_User user3, T_User user4]
minIncome = 2500



{-|
  The 'testExpr_True' function demonstrates the boolean expression functionality of the app.
  This functionality can be used to display information regarding to the data

  It can be used inside of the hIf attribute. The most simple form of an expression looks like this: data1 > data2. 
  Be careful. In front of the data you have to define the type by applying a constructor of the datatype 'template' (e. g. T_String x).
      T_Int data1 > T_Int data2.
  You can combine boolean expression by using logical operators ('AND', 'OR', 'NOT'). 
      T_Int data1 > T_Int data2 'AND' T_Int data3 < T_Int data4
-}
testExpr_True :: [HTMLValue]
testExpr_True = [html|<div hIf="T_String role1 != T_String role2 'AND' ('NOT' T_Int a > T_Int b)">Hello World</div>|]

testExpr_False :: [HTMLValue]
testExpr_False = [html|<div hIf="T_String role1 == T_String role2 'AND' ('NOT' T_Int a > T_Int b)">Hello World</div>|]


{-|
  The 'testPlaceholder' function demonstrates the placeholder functionality of the app.
  By using the placeholder functionality for a attribute you have to put the data inside curley braces (e.g id="{ T_String x }"). 
  By using the placeholder functionality for the content you have to put the data inside double curley braces (e.g {{ T_String x }}). 

  To define which datatype should be used by template haskell you have to specify a constructor of the datatype 'template' in front of 
        the data (e. g. T_String x).
  You can also apply some functions in front of the data. But the pattern should always look like this: 
        constructor function data (T_String lastName user1)
-}
testPlaceholder :: [HTMLValue]
testPlaceholder = [html|<div id="{ T_Int uid user1 }">
    {{ T_String greeting }} World {{ T_String greeting }}, my Name is {{ T_String firstName user1 }} {{ T_String lastName user1 }}</div>|]


{-|
  The 'testIteration' function demonstrates the iteration functionality. 
  It can be used to multiply html elements regarding to the provided list. 

  1. To use this functionality you first have to pack the list and all its entries with a constructor of the datatype 'template'.
        (T_List [T_User user1, T_User user2, T_User user3])
  2. After these preparations the data is ready to be used in the html template. The iteration pattern has to be specified right
        after the opening tag and before all attributes. 
        On the right side of the pattern the list is specified, on the left side the name of a single element.
        <div [group <- userGrouped]>
  3. The single element then can be used as a placeholder for templating. But this this time the element is already packed because of the
        preparation phase (phase 1). Because of that you have to write an unpack function (e.g. unpackUser), which then has to be applied to the data.
        Beside this special case the templating works like specified in the documentation of the testPlaceholder function. 
        {{ T_String firstName unpackUser user}}
-}
testIteration :: [HTMLValue]
testIteration = [html|<div [group <- userGrouped]><p [user <- group] id="{ T_Int uid unpackUser user }">
    Hello {{ T_String firstName unpackUser user }}</p></div>|]

{-|
  The 'testAllFeatures' function demonstrates all features in parallel. 
-}
testAllFeatures :: [HTMLValue]
testAllFeatures = [html|<div [u <- users] id="{ T_Int uid unpackUser u }" hIf="T_Int income unpackUser u > T_Int minIncome">
    My name is {{ T_String firstName unpackUser u }} {{ T_String lastName unpackUser u }}</div>|]

{-|
  The 'unpackUser' function unpacks a user. 
  For the moment this kind of function has to written manually by the user. 
  In future developments the function will be created automatically by template haskell. 
-}
unpackUser :: Template -> User 
unpackUser (T_User a) = a

{-|
  The 'testIO' function demonstrates how to write down the generated html to file. 
-}
testIO :: IO ()
testIO = renderHtml "test.html" testExpr_True