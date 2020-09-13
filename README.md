# Embedded Haskell (ehs)

## General

Embedded Haskell is a html templating engine for generating html documents depending on the provided data. It is based on two libraries. Parsec is used for implementing the Html Parser. And Template Haskell enables the possibility to generate code and providing variables.

## Features

### Expressions

#### General Explanation

- The Expression functionality can be used to hide and show information depending on the provided data.
- An expression has to to be assigned to a hIf attribute.
  - hIf="..."
- The most simple form of an expression looks similar to this:
  - data1 > data2.
  - !!! not valid !!!
- <b>Be careful.</b> In front of the data you have to define the type by applying a constructor of the datatype 'template'
  - e. g. T_String x.
  - T_Int data1 > T_Int data2.
- You can combine boolean expressions by using logical operators
  - 'AND', 'OR', 'NOT'
  - T_Int data1 > T_Int data2 'AND' T_Int data3 < T_Int data4

#### Example

    <div hIf="T_Int data1 > T_Int data2"></div>

### Placeholder

#### General Explanation

- The Placeholder functionality can be used to add content to a html element or to add a value to an html attribute.
- By using the placeholder functionality for a attribute you have to put the data inside curley braces
  - id="{ T_String x }"
- By using the placeholder functionality for the content you have to put the data inside double curley braces

  - {{ T_String x }}

- To define which datatype should be used by template haskell you have to apply a constructor of the datatype 'template' in front of
  the data
  - T_String x
- You can also apply some functions in front of the data. But the pattern should always look like this:
  - constructor function data
  - T_String lastName user1

#### Example

    <div class="{ T_Int uid } ">{{ T_String greeting }}</div>

### Iteration

#### General Explanation

The iteration functionality can be used to multiply html elements depending on the provided list.

1.  To use this functionality you first have to pack the list and all its entries with a constructor of the datatype 'template'.
    T_List [T_User user1, T_User user2, T_User user3]
2.  After these preparations the data is ready to be used in the html template. The iteration pattern has to be specified right after the opening tag and before all attributes. On the right side of the pattern the list is specified, on the left side the name of a single element.

        <div [group <- userGrouped]>

3.  The single element (left one) then can be used as a placeholder for templating. But this this time the element is already packed because of the preparation phase (phase 1). Because of that you have to write an unpack function (e.g. unpackUser), which then has to be applied to the data. Beside this special case the templating works like specified in the Placeholder Section.

        {{ T_String firstName unpackUser user}}

## Examples

- In the root directory run: <b>stack repl</b>

  - This starts the interpreter (ghci) and loads all modules of the src directory

- The test functions are implemented in the src/Html_Test.hs file.
  - To run the demo function for the expression functionality type in:
    - testExpr_True
    - testExpr_False
  - To run the demo function for the expression functionality type in:
    - testPlaceholder
  - To run the demo function for the iteration functionality type in:
    - testIteration
  - To run the demo function which shows all functionalities type in:
    - testAllFeatures
  - To run the demo function which shows the write-to-disk functionality type in:
    - testIO

## Project structure

### src

- src/Html_Parser.hs
  - contains all functions for parsing a html document
- src/Html_Data.hs
  - contains all necessary datatypes and instance declarations for parsing a html file
- src/Helper.hs
  - contains some helper functions
- src/Ehs.hs
  - contains functions for IO operations
  - e.g. for writing down the generated html to file
  - e.g. for initiation of the basic Template datatype
- src/Data.hs
  - contains an extended version of the basic implementation of the Template datatype
  - This implementation has to be maintained by the user.
- src/Html_Test.hs
  - contains some demos which show the functionality of the project
- src/Data.hs.txt
  - contains the basic implementation of the Template datatype which can be used in external projects.
  - This implementation has to be maintained by the user.

### How to use

1. Import the EHS module. This module provides all necessary functions for parsing a html string.
2. Create and import the Template datatype. The must have basic implementation can be found in the directory src/Data.hs.txt
   - This implementation has to be updated if you want to use a custom datatype
   - The src/Data.hs for example introduced the new constructor T_User for using the custom datatype User for templating
