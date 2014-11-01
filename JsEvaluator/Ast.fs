module Ast

type Statement =
    | ExpressionStmt of Expression
and Expression =
    | NumberLiteral of double

type Program = Program of Statement list