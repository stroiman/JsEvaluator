module EnvironmentSpec
open FSpec
open FSpec.Dsl
open FSpec.Matchers
open Runtime

type TestContext
  with
    member self.Environment 
      with get () : Environment = self?environment
      and set (value:Environment) = self?environment <- value

let specs = 
  describe "Environment" [
    before (fun ctx -> ctx.Environment <- Environment.Create ())

    describe ".getVariable()" [
      context "when variable doesn't exist" [
        it "returns 'undefined'" (fun c ->
          c.Environment.Get "foo" |> should (be.equalTo JsUndefined)
        )
      ]

      context "when variable exists" [
        before (fun x -> x.Environment.Add "key" (JsString "Foo"))

        it "returns the value" (fun c ->
          c.Environment.Get "key" |> should (be.equalTo (JsString "Foo")))
      ]
    ]
  ]
