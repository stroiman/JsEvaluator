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

    it "Parses + operators" (fun _ ->
      let expected = Program [ ExpressionStmt ( Plus ( NumberLiteral 2.0, NumberLiteral 1.0 ) ) ]
      "2 + 1" |> JsEval.parse |>should (be.equalTo expected))

    it "Parses - operators" (fun _ ->
      let expected = Program [ ExpressionStmt ( Minus ( NumberLiteral 2.0, NumberLiteral 1.0 ) ) ]
      "2 - 1" |> JsEval.parse |>should (be.equalTo expected))
  ]