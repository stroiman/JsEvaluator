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
    member self.ChildEnvironment 
      with get () : Environment = self?childEnvironment
      and set (value:Environment) = self?childEnvironment <- value

let foo = JsString "Foo"
let bar = JsString "Bar"

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
        before (fun x -> x.Environment.Add "key" foo)

        it "returns the value" (fun c ->
          c.Environment.Get "key" |> should (be.equalTo foo))
      ]

      context "when in a child environment" [
        before (fun x -> x.ChildEnvironment <- x.Environment |> Environment.CreateChild)

        context "when variable exists in parent environment" [
          before (fun x -> x.Environment.Add "key" foo)

          it "returns the value" (fun x ->
            x.ChildEnvironment.Get "key" |> should (be.equalTo foo))

          context "when different variable exists in child environment" [
            before (fun x -> x.ChildEnvironment.Add "key" bar)

            it "returns original value in parent environment" (fun x ->
              x.Environment.Get "key" |> should (be.equalTo foo))
          ]
        ]
      ]
    ]
  ]
