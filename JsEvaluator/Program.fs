﻿module JsEval
open Ast
open Runtime
open Lexer
open Parser
open Microsoft.FSharp.Text.Lexing

let token input =
    let result = Lexer.token input
    printfn "token: %A" result
    result

let parse input =
    let lexbuf = LexBuffer<_>.FromString input
    Parser.start token lexbuf

let eval (program : Program) =
  let evalStmt = function
    | ExpressionStmt (NumberLiteral x) -> JsNumber x

  match program with
  | Program [x] -> evalStmt x

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
