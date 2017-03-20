open AP.Ast
open System
open System.IO
open Microsoft.FSharp.Text.Lexing


[<EntryPoint>]
let main argv =
    let setInitialPos (lexbuf:LexBuffer<char>) filename =
        lexbuf.EndPos <- { pos_bol = 0;
                           pos_fname=filename;
                           pos_cnum=0;
                           pos_lnum=1 }
    use inputChannel = new StreamReader(File.OpenRead argv.[0])
    let code = inputChannel.ReadToEnd()
    AP.GlobalFunctions.PutCode(code)
    inputChannel.BaseStream.Position <- int64(0)
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromTextReader inputChannel
    let y = try 
                AP.Parser.start AP.Lexer.tokenize lexbuf
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
                failwith "Error";
    
    AP.Interpreter.Interpret y |> ignore
    Console.WriteLine("(press any key)")   
    Console.ReadKey(true) |> ignore
    0
    