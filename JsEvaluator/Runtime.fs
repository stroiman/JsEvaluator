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
    member self.Set name value =
        match self.data.ContainsKey name, self.parent with
        | true, _ -> self.Add name value
        | false, Some x -> x.Set name value
        | false, None -> failwith "Cannot set undefined variable"
    static member Create () = { parent = None; data = Map.empty }
    static member CreateChild x = { parent = Some x; data = Map.empty }
