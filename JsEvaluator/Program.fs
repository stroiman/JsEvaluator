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

let eval (env:Environment) (program : Program) =
  let rec evalExpr (env:Environment) = function
    | NumberLiteral x -> JsNumber x
    | Plus(x, y) ->
      match (evalExpr env x, evalExpr env y) with
      | JsNumber a, JsNumber b -> JsNumber (a + b)
      | _ -> failwith "Can only subtract numbers"
    | Minus(x, y) ->
      match (evalExpr env x, evalExpr env y) with
      | JsNumber a, JsNumber b -> JsNumber (a - b)
      | _ -> failwith "Can only subtract numbers"
    | VariableLookup x -> env.Get x

  let evalStmt (env:Environment) = function
    | ExpressionStmt x -> evalExpr env x
    | VariableDefinition (n,x) ->
        let value = evalExpr env x
        env.Add n value
        JsUndefined

  let rec evalStmtList (env:Environment) = function
    | [] -> JsUndefined
    | x::[] -> evalStmt env x
    | x::xs ->
      evalStmt env x |> ignore
      evalStmtList env xs 

  match program with
  | Program x -> evalStmtList env x

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
