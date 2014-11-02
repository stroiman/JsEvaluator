module EvalSpecs
open FSpec
open FSpec.Dsl
open FSpec.Matchers
open Runtime

let eval =
  let env = Environment.Create ()
  JsEval.parse >> JsEval.eval env

let specs =
  describe "Js evaluation" [
    context "variable lookup" [
      it "returns the variable value" (fun _ ->
        "var x = 42; x" |> eval
        |> should (be.equalTo (JsNumber 42.0)))
    ]

    context "when a single expression statement" [
      it "returns the value of the expression" (fun _ ->
        "5" |> eval |> should (be.equalTo (JsNumber 5.0)))
    ]

    context "when multiple expression statements" [
      it "returns the value of the last statement" (fun _ ->
        "5; 6" |> eval |> should (be.equalTo (JsNumber 6.0)))
    ]

    describe "arithmetic" [
      it "+ adds" (fun _ ->
        "2 + 1" |> eval |> should (be.equalTo (JsNumber 3.0)))

      it "- subtracts" (fun _ ->
        "2 - 1" |> eval |> should (be.equalTo (JsNumber 1.0)))
    ]
  ]