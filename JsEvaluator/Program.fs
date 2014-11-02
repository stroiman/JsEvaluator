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
  and evalStmt (env:Environment) = function
    | ExpressionStmt x -> evalExpr env x
    | VariableDefinition (n,x) ->
        let value = evalExpr env x
        env.Add n value
        JsUndefined
  and evalStmtList (env:Environment) = function
    | [] -> JsUndefined
    | ReturnStmt x::_ -> evalExpr env x
    | If(c,b)::rest ->
      match evalExpr env c with
      | JsBool(true) -> evalStmtList env (b@rest)
      | JsBool(false) -> evalStmtList env rest
      | x -> failwith "Not a boolean value: %A" x
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
