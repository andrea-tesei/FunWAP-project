namespace FunWAPLib

open AP.Ast
open AP.Lexer
open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Text.Lexing

module FunWap =
    // Function InterpretFunction: take as input the parameters of the FunctionCall and interpret it
    // The type checking phase is delegated to the caller
    let InterpretFunction (funCall :Dictionary<string,obj>) :string = 

        let addFunDeclarations (f :Fundecl list) =
            for i in 0..f.Length-1 do
                match f.Item(i) with
                | Fun(funz) -> AP.GlobalFunctions.AddFunction(funz.Name, f.Item(i)) |> ignore

        let setInitialPos (lexbuf:LexBuffer<char>) filename =
            lexbuf.EndPos <- { pos_bol = 0;
                           pos_fname=filename;
                           pos_cnum=0;
                           pos_lnum=1 }
        
        let code : string = funCall.Item("code") :?> string
        let nameFun : string = funCall.Item("nameFun") :?> string
        let paramz : List<AP.eval> = funCall.Item("params") :?> List<AP.eval>

        let mutable finalCode = code.Replace("\\n", "")
        finalCode <- finalCode.Replace("\\t", "")
        finalCode <- finalCode.Replace("\\r", "") 
        finalCode <- finalCode.Replace("\\", "")

        let ms = new MemoryStream()
        let w = new StreamWriter(ms)
        w.Write(finalCode.ToString())
        w.Flush()

        ms.Position <- int64(0)
        use s = new StreamReader(ms)
        let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromTextReader s
        let s1 = s.ReadToEnd()
        s.BaseStream.Position <- int64(0)
        let y = try 
                    AP.Parser.start tokenize lexbuf
                with e ->
                    let pos = lexbuf.StartPos
                    let line = pos.Line
                    let column = pos.Column
                    let message = e.Message
                    let lastToken = new System.String(lexbuf.Lexeme)
                    printf "Parse failed at line %d, column %d:\n" line column
                    printf "Last loken: %s" lastToken
                    printf "\n"
                    Console.WriteLine("(press any key)")   
                    Console.ReadKey(true) |> ignore
                    exit 1
        
        let mutable fundecls : Fundecl list = []
        match y with
        | Prog(pr) -> fundecls <- pr.Fundecs

        addFunDeclarations(fundecls)
        let mem = new AP.Env()
        mem.AddStack()
        let funOfCall = AP.GlobalFunctions.Lookup(nameFun)

        let mutable ret = AP.eval.Null
        match funOfCall with
        | Some(Fun(funz)) -> 
            for i in 0..funz.Params.Length-1 do
                match funz.Params.Item(i) with
                | Param(ide,t) -> 
                    match paramz.Item(i),t with
                        | (AP.eval.Int(n), TInt) -> mem.Extend(ide, AP.eval.Int(n)) |> ignore
                        | (AP.eval.Bool(b), TBool) -> mem.Extend(ide, AP.eval.Bool(b)) |> ignore
                        | _ -> failwith "error: the Dasync statement accept only Integer or Boolean as parameters of the function call designated"
            match funz.Block with
            | Block(decl,stmt) ->
                AP.Interpreter.InterpretDeclaration decl mem
                AP.Interpreter.InterpretStatement stmt mem
                match funz.Ret with
                | RetExpr(e1) ->
                    ret <- AP.Interpreter.InterpretExpr e1 mem
                | _ -> failwith "error: the Dasync statement accept only Expression as return type of the function call designated"
                    
        | None -> failwith "error: the Dasync statement accept only function that are defined globally"
        mem.RemoveStack()

        sprintf "%A" ret