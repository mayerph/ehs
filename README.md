# Embedded Haskell (ehs)

## Features

### Expressions

#### General Explanation

- The Expression functionality can be used to display information regarding to the provided data.
- It has to be used inside of a hIf attribute.
  - hIf="..."
- The most simple form of an expression looks similar to this:
  - data1 > data2.
  - !!! not valid !!!
- <b>Be careful.</b> In front of the data you have to define the type by applying a constructor of the datatype 'template'
  - e. g. T_String x.
  - T_Int data1 > T_Int data2.
- You can combine boolean expression by using boolean operators
  - 'AND', 'OR', 'NOT'
  - T_Int data1 > T_Int data2 'AND' T_Int data3 < T_Int data4

#### Example

        <div hIf="T_Int data1 > T_Int data2"></div>
