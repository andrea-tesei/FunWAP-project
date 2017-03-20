namespace AP

open System.Collections

exception UnboundVariable 

// Evaluation of a Program
type eval =
| Int of int
| Bool of bool
| Funval of efun // Closure
| IntUnbound
| BoolUnbound
| FunUnbound
| Null

// List of association between name and values (Environment)
and evalenv = (string * eval) list

// Expression function signature
and efun = {
    RetType : RType
    Params : Param list
    Block : Block
    Ret : RetFun
    Env : Env
}

// Type Env: it models the environment stack 
and public Env() =
    let mutable intEnv : ResizeArray<Map<string,eval>> = new ResizeArray<Map<string,eval>>()

    member x.GetInternalEnv() : ResizeArray<Map<string,eval>> = intEnv

    // Function addStack: push a new frame in the internal stack
    member x.AddStack() = 
        intEnv.Add(Map.empty)

    // Function removeStack: pop the last frame of the internal stack
    member x.RemoveStack() =
        intEnv.RemoveAt(intEnv.Count-1)

    member x.IsEmpty() : bool = if(intEnv.Count = 0) then 
                                    true 
                                else 
                                    false

    // Function lookup: searching for a bind for 's' variable
    member x.Lookup (s:string) = 
        let mutable auxiliar = Null
        let mutable index = -1
        for i = intEnv.Count-1 downto 0 do
            let envz = intEnv.Item(i)
            if intEnv.Item(i).TryFind(s) <> None then
                index <- i
                auxiliar <- intEnv.Item(i).Item(s)
        (index, auxiliar)

    // Function extend: insert new variable association in the current stack
    member x.Extend (s:string, a: eval) = intEnv.Item(intEnv.Count-1) <- intEnv.Item(intEnv.Count-1).Add(s,a)

    // Function updateVal: update the values associated to 's' variable in the Environment
    member x.UpdateVal (s:string, an: eval) = 
        let index, value = x.Lookup(s)
        intEnv.Item(index) <- intEnv.Item(index).Remove(s)
        intEnv.Item(index) <- intEnv.Item(index).Add(s,an)

// Module GlobalFunctions: it models the Environment of global function declaration in the program (e.g. no 'main' function)
module GlobalFunctions = 
    begin 
    
    // Map of name and code of each global function
    let mutable intEnv : Map<string,Fundecl> = Map.empty
    
    // Whole source code in input (used in the DAsync construct (see Interpret.fs))
    let mutable codeAsString : string = ""

    let PutCode (code :string) = codeAsString <- code
    let GetCode () = codeAsString

    let GetInternalEnv() : Map<string,Fundecl> = intEnv

    // Add new function in the Global function list
    let AddFunction(x:string, func:Fundecl) = intEnv <- intEnv.Add(x,func)
    
    let IsEmpty = intEnv.IsEmpty

    // Lookup for a Fundecl named as 'x'
    let Lookup (x:string) = 
        intEnv.TryFind(x)

    // Update the fundecl associated to the function 'x', if exists
    let UpdateVal (x:string, an: Fundecl) (env :evalenv) =
        if intEnv.TryFind(x) <> None then
            intEnv <- intEnv.Remove(x) 
            intEnv <- intEnv.Add(x,an) 
    end

