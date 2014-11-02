module Ast

type Statement =
    | ExpressionStmt of Expression
and Expression =
    | NumberLiteral of double
    | Plus of Expression * Expression
    | Minus of Expression * Expression

type Program = Program of Statement list
