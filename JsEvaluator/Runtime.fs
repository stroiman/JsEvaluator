module Runtime

type JsVariableType =
  | JsUndefined
  | JsString of string

type Environment = {
    parent : Environment option
    mutable data : Map<string,JsVariableType>
  }
  with
    member self.Add name value = 
        self.data <- self.data.Add (name,value)
    member self.Get name = 
        match self.data.TryFind name, self.parent with
        | Some x, _ -> x
        | None, Some p -> p.Get name
        | None, None -> JsUndefined
    static member Create () = { parent = None; data = Map.empty }
    static member CreateChild x = { parent = Some x; data = Map.empty }
