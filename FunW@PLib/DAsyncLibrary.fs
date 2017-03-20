namespace FunWAPLib

open AP.Ast
open AP.Lexer
open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Text.Lexing

module FunWap =
    
    // Funzione start: prende in ingresso i parametri della FunctionCall ed effettua l'interpretazione
    // la fase di typechecking viene delegata al chiamante
    let start (funCall :Dictionary<string,obj>) :string = 

        let addFunDeclarations (f :Fundecl list) =
            for i in 0..f.Length-1 do
                match f.Item(i) with
                | Fun(funz) -> AP.GlobalFunctions.addFunction(funz.Name, f.Item(i)) |> ignore

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
        setInitialPos(lexbuf) "provacode.txt" |> ignore
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
        mem.addStack()
        let funOfCall = AP.GlobalFunctions.lookup(nameFun)

        let mutable ret = AP.eval.Null
        match funOfCall with
        | Some(Fun(funz)) -> 
            for i in 0..funz.Params.Length-1 do
                match funz.Params.Item(i) with
                | Param(ide,t) -> 
                    match paramz.Item(i),t with
                        | (AP.eval.Int(n), TInt) -> mem.extend(ide, AP.eval.Int(n)) |> ignore
                        | (AP.eval.Bool(b), TBool) -> mem.extend(ide, AP.eval.Bool(b)) |> ignore
                        | _ -> failwith "error: the Dasync statement accept only Integer or Boolean as parameters of the function call designated"
            match funz.Block with
            | Block(decl,stmt) ->
                AP.Interpreter.interpretDeclaration decl mem
                AP.Interpreter.interpretStatement stmt mem
                match funz.Ret with
                | RetExpr(e1) ->
                    ret <- AP.Interpreter.interpretExpr e1 mem
                | _ -> failwith "error: the Dasync statement accept only Expression as return type of the function call designated"
                    
        | None -> failwith "error: the Dasync statement accept only function that are defined globally"
        mem.removeStack()

        sprintf "%A" ret