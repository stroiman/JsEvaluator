module JsEval
open Ast
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

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
