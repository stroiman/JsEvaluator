module Runtime

type JsVariableType =
  | JsUndefined
  | JsString of string

type Environment = {
    mutable data : Map<string,JsVariableType>
  }
  with
    member self.Add name value = 
        self.data <- self.data.Add (name,value)
    member self.Get name = 
        match self.data.TryFind name with
        | Some x -> x
        | None -> JsUndefined
    static member Create () = { data = Map.empty }