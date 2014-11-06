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

let eval (env:Environment) (Program statementList) =
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
          match fObj with
          | JsFunction (funcEnv,argNames,body) ->
              // When a function is called, create a new Environment that is
              // the child of the environment _in which the function was defined_.
              let nestedEnv = funcEnv |> Environment.CreateChild
              // Evaluate the value for the parameters of the function
              let parameters = ps |> List.map (evalExpr env)
              // Add them to this nested frame, using the arguments' names as keys.
              let addParameterToEnvironment (arg,parameter) : unit = nestedEnv.Add arg parameter
              // Just zip the argument names with parameter values, will fail
              // if the two lists don't have the same length. 
              let argValuePairs = List.zip argNames parameters
              List.iter addParameterToEnvironment argValuePairs
              // Evaluate the function body using the new environment
              evalStmtList nestedEnv body
          | x -> failwithf "Not a function: %A" x
    and evalStmtList (env:Environment) =
        // Evaluate the result of a statement list. The last value produced
        // by a statement becomes the return value for the statement list if
        // no explicit return statement is specified.
        // The simplest way to accomplish this is call the function 
        // tail-recursively with the value of the last evaluated value as
        // a parameter, and return that value when we reach the end of the list.
        let rec iter lastValue = function
          | [] -> lastValue
          | x::xs ->
              match x with
              | ReturnStmt y -> evalExpr env y
              | If(cond,body) ->
                  match evalExpr env cond with
                  | JsBool(true) -> 
                      // If the expression is true, generate a new statement list
                      // consisting of the if's body and the rest of the evaluated
                      // list.
                      iter lastValue (body@xs)
                  | JsBool(false) -> iter lastValue xs
                  | x -> failwith "Not a boolean value: %A" x
              | ExpressionStmt x -> iter (evalExpr env x) xs
              | VariableDefinition (n,x) ->
                  let value = evalExpr env x
                  env.Add n value
                  iter lastValue xs
        iter JsUndefined

    evalStmtList env statementList

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
