module JsEval
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
  let rec evalExpr = function
    | NumberLiteral x -> JsNumber x
    | Plus(x, y) ->
      match (evalExpr x, evalExpr y) with
      | JsNumber a, JsNumber b -> JsNumber (a + b)
      | _ -> failwith "Can only subtract numbers"
    | Minus(x, y) ->
      match (evalExpr x, evalExpr y) with
      | JsNumber a, JsNumber b -> JsNumber (a - b)
      | _ -> failwith "Can only subtract numbers"

  let evalStmt = function
    | ExpressionStmt x -> evalExpr x

  let rec evalStmtList = function
    | [] -> JsUndefined
    | x::[] -> evalStmt x
    | x::xs ->
      evalStmt x |> ignore
      evalStmtList xs 

  match program with
  | Program x -> evalStmtList x

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
