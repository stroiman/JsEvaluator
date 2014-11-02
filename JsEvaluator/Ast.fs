module Ast

type Statement =
    | ExpressionStmt of Expression
    | VariableDefinition of string * Expression
    | ReturnStmt of Expression
    | If of Expression * Statement list
and Expression =
    | VariableLookup of string
    | NumberLiteral of double
    | Plus of Expression * Expression
    | Minus of Expression * Expression
    | LessThanOrEqual of Expression * Expression
    | FunctionDefinition of string list * Statement list
    | FunctionInvocation of Expression * Expression list

type Program = Program of Statement list
