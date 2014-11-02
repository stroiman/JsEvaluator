module JsEval
open Ast
open Runtime
open Lexer
open Parser
open Microsoft.FSharp.Text.Lexing

let token input =
    let result = Lexer.token input
//    printfn "token: %A" result
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
    | LessThanOrEqual (x,y) ->
      match evalExpr env x, evalExpr env y with
      | JsNumber a, JsNumber b -> JsBool (a <= b)
      | _ -> failwithf "Can only compare numbers"
    | VariableLookup x -> env.Get x
    | FunctionDefinition(args, body) ->
        JsFunction(env,args, body)
    | FunctionInvocation(f,ps) ->
        let fObj = evalExpr env f
        let parameters = ps |> List.map (evalExpr env)
        match fObj with
        | JsFunction (env,args,body) ->
            let nestedEnv = env |> Environment.CreateChild
            let add (a,p) : unit = nestedEnv.Add a p
            let argValuePaies = List.zip args parameters
            List.iter add argValuePaies
            evalStmtList nestedEnv body
        | x -> failwithf "Not a function: %A" x
    | x -> failwithf "Cannot evaluate %A" x
  and evalStmtList (env:Environment) =
    let rec iter last = function
      | [] -> last
      | x::xs ->
        match x with
        | ReturnStmt y -> evalExpr env y
        | If(c,b) ->
          match evalExpr env c with
          | JsBool(true) -> evalStmtList env (b@xs)
          | JsBool(false) -> evalStmtList env xs
          | x -> failwith "Not a boolean value: %A" x
        | ExpressionStmt x -> iter (evalExpr env x) xs
        | VariableDefinition (n,x) ->
            let value = evalExpr env x
            env.Add n value
            iter last xs
    iter JsUndefined

  match program with
  | Program x -> evalStmtList env x

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
