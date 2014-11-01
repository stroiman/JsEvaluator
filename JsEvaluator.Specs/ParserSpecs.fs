module ParserSpecs
open FSpec
open FSpec.Dsl
open FSpec.Matchers
open Ast

let specs =
  describe "Parse" [
    it "Parses a single number" (fun _ ->
      let expected = Program [ ExpressionStmt ( NumberLiteral 42.0 )]
      "42" |> JsEval.parse
      |> should (be.equalTo expected))
  ]