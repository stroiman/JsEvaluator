module Ast

type Statement =
    | ExpressionStmt of Expression
    | VariableDefinition of string * Expression
and Expression =
    | VariableLookup of string
    | NumberLiteral of double
    | Plus of Expression * Expression
    | Minus of Expression * Expression
    | FunctionDefinition of string list * Statement list

type Program = Program of Statement list
