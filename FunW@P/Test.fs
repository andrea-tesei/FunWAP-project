open AP.FunWAP
open System
open System.IO
open Microsoft.FSharp.Text.Lexing


[<EntryPoint>]
let main argv = 
    let x = "   
        fun main() {
                var diocaro int = 4;
        }  
    "   

    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString x   

    while not lexbuf.IsPastEndOfStream do  
        printfn "%A" (AP.Lexer.tokenize lexbuf)   
 
    Console.WriteLine("(press any key)")   
    Console.ReadKey(true) |> ignore
    0