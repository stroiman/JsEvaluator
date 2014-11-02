module ParserSpecs
open FSpec
open FSpec.Dsl
open FSpec.Matchers
open Ast

let parseSingleStatement =
  JsEval.parse >> function
  | Program[x] -> x
  | _ -> failwith "expected program with one statement"

let parseSingleExpression =
  parseSingleStatement >> function
  | ExpressionStmt x -> x
  | _ -> failwith "expected an expression"

let specs =
  describe "Parse" [
    it "parses a function" (fun _ ->
      let expected =
        FunctionDefinition (
          ["a"; "b"],
          [ExpressionStmt (VariableLookup "a")
           ExpressionStmt (VariableLookup "b")])
      "function(a,b) { a; b }"
      |> parseSingleExpression
      |> should (be.equalTo expected))
    it "Parses a single number" (fun _ ->
      let expected = NumberLiteral 42.0
      "42" |> parseSingleExpression
      |> should (be.equalTo expected))

    it "Parses + operators" (fun _ ->
      let expected = Plus ( NumberLiteral 2.0, NumberLiteral 1.0 ) 
      "2 + 1" |> parseSingleExpression |>should (be.equalTo expected))

    it "Parses - operators" (fun _ ->
      let expected = Minus ( NumberLiteral 2.0, NumberLiteral 1.0 ) 
      "2 - 1" |> parseSingleExpression |>should (be.equalTo expected))

    it "Parses multiple statements" (fun _ ->
      let expected = 
        Program [
          ExpressionStmt (NumberLiteral 1.0)
          ExpressionStmt (NumberLiteral 2.0)
        ]
      "1; 2" |> JsEval.parse |> should (be.equalTo expected))

    it "Parses variable definition" (fun _ ->
      let expected = VariableDefinition("x", NumberLiteral 42.0)
      "var x = 42" |> parseSingleStatement |> should (be.equalTo expected))

    it "Parses variable lookup" (fun _ ->
      let expected = VariableLookup "x"
      "x" |> parseSingleExpression |> should (be.equalTo expected))
  ]