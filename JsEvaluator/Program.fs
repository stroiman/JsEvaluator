﻿module JsEval
open Ast
open Lexer
open Parser
open Microsoft.FSharp.Text.Lexing

let parse input =
    let lexbuf = LexBuffer<_>.FromString input
    Parser.start Lexer.token lexbuf

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
