namespace AP

open System
open System.Text

// Module Interpreter: it performs the interpretation of a given Program (composed by declarations and statements)
module Interpreter =
    
    // Function InterpretExpr: it performs the interpretation of the given Expression 'e' in the Environment 'Env'
    let rec InterpretExpr (e:Expr) (env:Env) : eval =
        match e with
        // Interpret Addition
        | Sum(s1, s2) -> 
            let a1 = InterpretExpr s1 env
            let a2 = InterpretExpr s2 env
            match (a1,a2) with
            | (Int(n1), Int(n2)) -> Int(n1 + n2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret Subtraction
        | Sub(s1, s2) -> 
            let a1 = InterpretExpr s1 env
            let a2 = InterpretExpr s2 env
            match (a1,a2) with
            | (Int(n1), Int(n2)) -> Int(n1 - n2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret Division
        | Div(s1, s2) -> 
            let a2 = InterpretExpr s1 env
            let a1 = InterpretExpr s2 env
            if a2 <> Int(0) then
                match (a1,a2) with
                | (Int(n1), Int(n2)) -> Int(n1 / n2)
                | _ -> failwith "error: type mismatch in expression"
            else
                failwith "error: division by 0" 
        // Interpret Product
        | Prod(p1, p2) -> 
            let a1 = InterpretExpr p1 env 
            let a2 = InterpretExpr p2 env
            match (a1,a2) with
            | (Int(n1), Int(n2)) -> Int(n1 * n2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 > e2'
        | Greater(e1, e2) -> 
            let a1 = InterpretExpr e1 env
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Int(b1), Int(b2)) -> Bool(b1 > b2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 >= e2'
        | GreaterEQ(e1,e2) ->
            let a1 = InterpretExpr e1 env
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Int(b1), Int(b2)) -> Bool(b1 >= b2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 < e2'            
        | Lower(e1, e2) -> 
            let a1 = InterpretExpr e1 env
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Int(b1), Int(b2)) -> Bool(b1 < b2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 <= e2'
        | LowerEQ(e1, e2) -> 
            let a1 = InterpretExpr e1 env 
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Int(b1), Int(b2)) -> Bool(b1 <= b2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 != e2'
        | Differ(e1, e2) ->
            let a1 = InterpretExpr e1 env
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 <> b2)
            | (Int(n1), Int(n2)) -> Bool(n1 <> n2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 == e2'
        | Equal(e1, e2) -> 
            let a1 = InterpretExpr e1 env
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 = b2)
            | (Int(n1), Int(n2)) -> Bool(n1 = n2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 && e2'
        | And(e1, e2) -> 
            let a1 = InterpretExpr e1 env
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 && b2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula 'e1 || e2'
        | Or(e1, e2) -> 
            let a1 = InterpretExpr e1 env
            let a2 = InterpretExpr e2 env
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 || b2)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret boolean formula '!e1'
        | Not(e1) -> 
            let a1 = InterpretExpr e1 env
            match a1 with
            | Bool(b1) -> Bool(not b1)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret negative integer value
        | Minus(e1) ->
            let a1 = InterpretExpr e1 env
            match a1 with
            | Int(n1) -> Int(-n1)
            | _ -> failwith "error: type mismatch in expression"
        // Interpret Variable 's'
        | Var(s) -> let index, value = env.Lookup(s)
                    if (index,value) <> (-1, Null) then
                        value
                    else 
                        let s1 = "error: variable \"\" is not defined"
                        failwith (s1.Insert(17, s))
        // Interpret Expression with parenthesis
        | ParExp(e1) -> InterpretExpr e1 env
        // Interpret Call Expression: parameters association and call
        | Call(ide, elist) -> 
            // Check if this function is already declared
            let lookfun = GlobalFunctions.Lookup(ide)
            let mutable ret = Null
            match lookfun with
            // Function exists
            | Some(Fun(funz)) ->
                // Add new stack for parameters
                let envfun = new Env()
                envfun.AddStack()
                // Interpret actual parameters
                let reslist = elist |> List.map(fun e1 -> InterpretExpr e1 env)
                for i in 0..funz.Params.Length-1 do
                    match funz.Params.Item(i) with
                    // Associate formal parameters with actual ones
                    | Param(ide, t) -> 
                        match reslist.Item(i), t with
                        | (Int(n), TInt) -> envfun.Extend(ide, Int(n)) |> ignore
                        | (Bool(b), TBool) -> envfun.Extend(ide, Bool(b)) |> ignore
                        | (Funval(efun), TFun) -> envfun.Extend(ide, Funval(efun)) |> ignore
                        | _ -> 
                            let s = "error: type mismatch in function \"\""
                            failwith (s.Insert(s.Length-1, funz.Name))
                // Interpret the Block of the function 
                match funz.Block with
                | Block(decl, stmt) -> 
                    // Performs declarations in the block
                    InterpretDeclaration decl envfun
                    // Execute statements in the block
                    InterpretStatement stmt envfun
                    match funz.Ret with
                    // Return value is an eval
                    | RetExpr(e1) ->
                        // Interpret the expression e1 in the Environment resulted from block's execution 
                        ret <- InterpretExpr e1 envfun
                        // Pop the stack with function parameters
                        envfun.RemoveStack()
                    // Return value is an anonymous function
                    | AFun(anonymous) -> 
                        // Prepare the return anonymous function with the Environment resulted from block's execution
                        ret <- Funval({RetType = anonymous.TypeRet; Params = anonymous.Params; Block = anonymous.Block; Ret = anonymous.Ret; Env = envfun})
                // type checking
                match ret, funz.TypeRet with
                | (Int(n), FInt) -> ret
                | (Bool(b), FBool) -> ret
                | (Funval(efun), FFun(l,t)) -> ret
                | _ -> 
                    let s = "error: type mismatch in function call \"\""
                    failwith (s.Insert(s.Length-1, funz.Name))
            | None -> 
                // No global function declaration found
                // Check if the function has been declared not globally
                let index, afun = env.Lookup(ide)
                if afun <> Null then
                    match afun with
                    // Interpret the function found: same as above
                    | Funval(efun) ->
                        let reslist = List.map(fun e1 -> InterpretExpr e1 env) elist
                        efun.Env.AddStack()
                        for i in 0..efun.Params.Length-1 do
                            match efun.Params.Item(i) with
                            | Param(ide, t) -> 
                                match reslist.Item(i), t with
                                | (Int(n), TInt) -> efun.Env.Extend(ide, Int(n)) |> ignore
                                | (Bool(b), TBool) -> efun.Env.Extend(ide, Bool(b)) |> ignore
                                | (Funval(efun), TFun) -> efun.Env.Extend(ide, Funval(efun)) |> ignore
                                | _ -> 
                                    let s = "error: type mismatch in function call \"\""
                                    failwith (s.Insert(s.Length-1, ide))
                        match efun.Block with
                        | Block(decl, stmt) -> 
                            InterpretDeclaration decl efun.Env
                            InterpretStatement stmt efun.Env
                            match efun.Ret with
                            | RetExpr(e1) -> 
                                ret <- InterpretExpr e1 efun.Env
                                efun.Env.RemoveStack()
                            | AFun(anonymous) -> 
                                ret <- Funval({RetType = anonymous.TypeRet; Params = anonymous.Params; Block = anonymous.Block; Ret = anonymous.Ret; Env = efun.Env})
                        

                        match ret, efun.RetType with
                        | (Int(n), FInt) -> ret
                        | (Bool(b), FBool) -> ret
                        | (Funval(efun), FFun(l,t)) -> ret
                        | _ -> 
                            let s = "error: type mismatch in function call \"\""
                            failwith (s.Insert(s.Length-1, ide))
                    | _ -> 
                        let s = "error: variable \"\" is not a function"
                        failwith (s.Insert(17, ide))
                else
                    let s = "error: no previous declaration of function \"\""
                    failwith (s.Insert(s.Length-1, ide))
        // Interpret Asynchronously the given expression ('async' contruct in F#)
        | Async(e1) -> 
            let ret = ref Null
            let asyncblock = async { ret := (InterpretExpr e1 env) }
            Async.RunSynchronously asyncblock
            !ret
        // Interpret Asynchronously and remotely the given expression
        | DAsync(ide,e1) -> 
            match e1 with
            | Call(ide1, elist) -> 
                let listEvals = elist |> List.map(fun e1 -> InterpretExpr e1 env)
                let code = AP.GlobalFunctions.GetCode()
                let mutable ret = AP.WebServiceCLI.DAsyncPOST (ide) (code) (ide1) (listEvals)
                ret <- ret.Replace("\\","")
                ret <- ret.Replace("\"", "")
                let mutable realret = Null
                // Parse the result
                if(ret.Contains("Int")) then
                    ret <- ret.Replace("Int", "")
                    realret <- Int(int(Convert.ToInt64(ret)))
                else if (ret.Contains("Bool")) then
                    if(ret.Contains("true")) then
                        realret <- Bool(true)
                    else 
                        realret <- Bool(false)
                // Type Checking for result
                let fundef = AP.GlobalFunctions.Lookup(ide1)
                match fundef with
                | Some(Fun(funz)) ->
                    match funz.TypeRet, realret with
                    | (FBool, Bool(b)) -> realret
                    | (FInt, Int(n)) -> realret
                    | _ -> 
                        let s = "error: type mismatch in Dasync. Function call name: \"\""
                        failwith (s.Insert(s.Length-1, ide))
                | None -> 
                    let s = "error: no global declaration of function \"\""
                    failwith (s.Insert(s.Length-1, ide))
            | _ -> failwith("error: the Dasync statement accept only \"Function Call\" as return type")
        // Interpret boolean 'true' or 'false'
        | Boolean(b) ->  
            match b with
            | false -> Bool(false)
            | true -> Bool(true)
        // Interpret Integer value
        | Num(n) -> Int(n)
    
    // Function InterpretDeclaration: interpret 'decls' declaration list in the given 'env' Environment
    and InterpretDeclaration (decls : Decl list) (env : Env) =
        for i in 0..decls.Length-1 do
            match decls.Item(i) with
            // Interpret declaration + assignment
            | DecAssign(ide, t, e1) ->
                let index,value = env.Lookup(ide) 
                if value = Null then
                    // no previous declaration
                    let res = InterpretExpr e1 env
                    match t,res with
                    | (TBool, Bool(b)) -> env.Extend(ide, res) |> ignore
                    | (TInt, Int(n)) -> env.Extend(ide, res) |> ignore
                    | (TFun, Funval(efun)) -> env.Extend(ide, res) |> ignore
                    | (_,_) -> 
                        let s = "error: type mismatch in declaration of variable \"\""
                        failwith (s.Insert(s.Length-1, ide))
                else
                    let s = "error: there is a previous declaration of variable \"\""
                    failwith (s.Insert(s.Length-1,ide))                    
            // Interpret declaration
            | Dec(ide, t) -> 
                let index,value = env.Lookup(ide) 
                if value = Null then
                    // no previous declaration
                    match t with
                    | TBool -> env.Extend(ide, BoolUnbound) |> ignore
                    | TInt -> env.Extend(ide, IntUnbound) |> ignore
                    | TFun -> env.Extend(ide, FunUnbound) |> ignore
                else
                    let s = "error: there is a previous declaration of variable \"\""
                    failwith (s.Insert(s.Length-1,ide))

    // Function InterpretStatement: interpret the given statement 'stmts' in the given Environment 'env'
    and InterpretStatement (stmts : Stmt list) (env : Env) =
        for i in 0..stmts.Length-1 do
            match stmts.Item(i) with
            // Interpret Assignment
            | Assign(ide, e1) -> 
                let s,precv = env.Lookup(ide)
                if precv <> Null then
                    let res = InterpretExpr e1 env
                    match precv,res with
                    | (Int(n), Int(v)) -> env.UpdateVal(ide, res)
                    | (Bool(b1), Bool(b2)) -> env.UpdateVal(ide, res)
                    | (Funval(f1), Funval(f2)) -> env.UpdateVal(ide, res)
                    | (IntUnbound, Int(v)) -> env.UpdateVal(ide, res)
                    | (BoolUnbound, Bool(b)) -> env.UpdateVal(ide, res)
                    | (FunUnbound, Funval(f)) -> env.UpdateVal(ide, res)
                    | (_,_) -> 
                        let s = "error: type mismatch in assignment of variable \"\""
                        failwith (s.Insert(s.Length-1, ide))
                else
                    let s = "error: undefined reference. No previous declaration of variable \"\""
                    failwith (s.Insert(s.Length-1,ide))
            // Interpret if-then-else 
            | IfThenElse(e1, bthen, belse) -> 
                let condres = InterpretExpr e1 env
                env.AddStack()
                if condres = Bool(true) then
                    match bthen with
                    | Block(thendec, thenst) -> 
                        InterpretDeclaration thendec env
                        InterpretStatement thenst env
                else
                    match belse with
                    | Block(elsedec, elsest) ->
                        InterpretDeclaration elsedec env
                        InterpretStatement elsest env
                env.RemoveStack()
            // Interpret if-then
            | IfThen(e1, bthen) -> 
                let condres = InterpretExpr e1 env
                env.AddStack()
                if condres = Bool(true) then
                    match bthen with
                    | Block(thendec, thenst) -> 
                        InterpretDeclaration thendec env
                        InterpretStatement thenst env
                env.RemoveStack()
            // Interpret for statement
            | For(forstmt) -> 
                let mutable condres = InterpretExpr forstmt.Cond env
                env.AddStack()
                let init = InterpretStatement [forstmt.Init] env
                while condres.Equals(Bool(true)) do
                    match forstmt.Block with
                    | Block(decl, stmt) -> 
                        InterpretDeclaration decl env
                        InterpretStatement stmt env
                        InterpretStatement [forstmt.Update] env
                        condres <- InterpretExpr forstmt.Cond env
                env.RemoveStack() 
            // Interpret println statement
            | Println(e1) -> 
                let res = InterpretExpr e1 env
                printfn "%A" res

    // Function AddFunDeclarations: it add the given function declaration list to the global functions
    let AddFunDeclarations (f :Fundecl list) =
        for i in 0..f.Length-1 do
            match f.Item(i) with
            | Fun(funz) -> GlobalFunctions.AddFunction(funz.Name, f.Item(i)) |> ignore

    // Function Interpret: interpret a given program 'p' and return the resulted Environment
    let rec Interpret(p:Prog) : AP.Env =
        let mutable fundecls : Fundecl list = []
        let mutable main : Block = Block([],[])
        let mem = new Env()
        match p with 
        | Prog(pr) -> main <- pr.Main
                      fundecls <- pr.Fundecs
                      if not(fundecls.IsEmpty) then
                        AddFunDeclarations(fundecls)
                      mem.AddStack()
                      match main with
                      | Block([],[]) -> failwith "error: no statement or declaration in main function"
                      | Block([],stmts) -> 
                            InterpretStatement stmts mem
                            mem
                      | Block(decls, stmts) -> 
                            InterpretDeclaration decls mem
                            InterpretStatement stmts mem
                            mem
        
