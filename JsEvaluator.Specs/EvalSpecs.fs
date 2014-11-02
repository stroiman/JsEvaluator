module EvalSpecs
open FSpec
open FSpec.Dsl
open FSpec.Matchers
open Runtime

let eval =
  JsEval.parse >> JsEval.eval

let specs =
  describe "Js evaluation" [
    context "when a single expression statement" [
      it "returns the value of the expression" (fun _ ->
        "5" |> eval |> should (be.equalTo (JsNumber 5.0)))
    ]

    context "when multiple expression statements" [
      it "returns the value of the last statement" (fun _ ->
        "5; 6" |> eval |> should (be.equalTo (JsNumber 6.0)))
    ]
  ]