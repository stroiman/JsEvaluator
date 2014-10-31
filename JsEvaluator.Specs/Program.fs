// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<MbUnit.Framework.TestFixture>]
type Wrapper() =
    inherit FSpec.MbUnitWrapper.MbUnitWrapperBase()

[<EntryPoint>]
let main argv = 
    System.Reflection.Assembly.GetExecutingAssembly ()
    |> FSpec.TestDiscovery.runSingleAssembly
