open System
open System.IO
open System.Text.RegularExpressions
 
// FunW@P grammar
(*  E ::= P E1 | C
    C :: = Let Ide = E2 in E | epsilon
    E1 ::= + PE1 | - PE1 | epsilon
    P ::= Term P1 | Term B 
    P1 ::=  * P | / P | epsilon
    B ::= < E | > E | <> E | = E | and E | or E | epsilon 
    Term ::= Num | true | false | not E | If E then E else E end | (E) | Ide | epsilon
    Ide ::= char | char Ide 
    Num ::= digit | digit Num
    char is \S+
    digit is \d+ *)
 
type eval =
| Int of int
| Bool of bool
| Unbound

module Env = 
    begin 
    type evalenv = (String * eval) list
    exception Emptyenv
    exception UnboundVariable
    
    let mutable intEnv : evalenv = []

    let emptystack(): evalenv = []
   
    let getInternalEnv() : evalenv = intEnv

    let cons ((s,a)) = intEnv <- (s,a)::intEnv
   
    let empty = 
            match intEnv with
            | (s,a)::p -> false
            | [] -> true

    let rec lookup (x:String) (env :evalenv) = 
        match env with 
        | [] -> raise UnboundVariable
        | (s,a)::p -> if x = s then a else lookup x p

    let extend (x:String, a: eval) = cons (x,a) 
    end

type Token =
| Plus
| Minus
| Div
| TGreater
| TLower
| TEqual
| TDifferent
| TAnd
| TOr
| TNot
| TLet
| TIn
| If
| Then
| Else
| End
| Mul
| OpenBrace
| ClosedBrace
| Num of int
| TBoolean of bool
| TVar of String
| EOF
| BOF
 
type Tokenizer(s:StreamReader) =
  let mutable lastToken = BOF
  let eatChar () = s.Read() |> ignore
 
  let rec eatWS () =
    if s.EndOfStream || char(s.Peek()) <> ' ' then ()
    else eatChar(); eatWS()
  
  let rec parseInt(n) =
    if s.EndOfStream then n
    else
      let c = s.Peek()
      if c >= int('0') && c <= int('9') then
        eatChar()
        parseInt(n * 10 + (c - int('0')))
      else
        n

  let rec parseVar(aux) =
    if s.EndOfStream then aux
    else
        let c = s.Peek()
        if Regex("[a-zA-Z0-9-_]+").Match("" + char(c).ToString()).Success then
            eatChar();
            eatWS();
            parseVar(aux + char(c).ToString())
        else
            aux
 
  member this.CurrentToken with get() = lastToken
 
  member this.NextToken () : Token =
    
    lastToken <-
      if s.EndOfStream then
        EOF
      else
        eatWS()
        if s.EndOfStream then
          EOF
        else
          let c = char(s.Peek())
          match c with
          | '+' -> eatChar();
                   eatWS()
                   let z = char(s.Peek()) 
                   if char(s.Peek()) = '-' || char(s.Peek()) = '+' || char(s.Peek()) = '(' || char(s.Peek()) = 'i' then
                        Plus
                   else
                        let x = parseInt(0)
                        Num(x)
                       
          | '-' -> eatChar(); 
                   eatWS()
                   let z = char(s.Peek())
                   if char(s.Peek()) = '-' || char(s.Peek()) = '+' || char(s.Peek()) = '(' || char(s.Peek()) = 'i' then
                        Plus
                   else
                        let x = -parseInt(0)
                        Num(x)
          | '/' -> eatChar(); Div
          | '<' -> eatChar();
                   if char(s.Peek()) = '>' then
                        eatChar(); TDifferent
                   else
                        TLower 
          | '>' -> eatChar(); TGreater
          | '=' -> eatChar(); TEqual
          | '&' -> eatChar();
                   if char(s.Peek()) = '&' then
                        eatChar(); TAnd
                   else
                        failwith "Syntax Error"
          | '|' -> eatChar();
                   if char(s.Peek()) = '|' then
                        eatChar(); TOr
                   else
                        failwith "Syntax Error"
          | '*' -> eatChar(); Mul
          | '(' -> eatChar(); OpenBrace
          | ')' -> eatChar(); ClosedBrace
          | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
            Num(parseInt(0))
          | 'l' -> 
            eatChar();
            match char(s.Peek()) with
            | 'e' ->
                eatChar();
                if char(s.Peek()) = 't' then
                    eatChar(); TLet
                else
                    let var = parseVar("le")
                    TVar(var)

            | _ -> let var = parseVar("l")
                   TVar(var)
          | 'i' ->
            eatChar();
            if char(s.Peek()) = 'f' then
                eatChar(); If
            else
                if char(s.Peek()) = 'n' then
                    eatChar(); TIn
                else
                    let var = parseVar("i")
                    TVar(var)
          | 't' ->
            eatChar();
            match char(s.Peek()) with
            | 'h' ->
                eatChar();
                if char(s.Peek()) = 'e' then
                    eatChar();
                    if char(s.Peek()) = 'n' then
                        eatChar(); Then
                    else
                        let var = parseVar("the")
                        TVar(var)
                else
                    let var = parseVar("th")
                    TVar(var)
            | 'r' -> 
                eatChar();
                if char(s.Peek()) = 'u' then
                    eatChar();
                    if char(s.Peek()) = 'e' then
                        eatChar(); TBoolean(true)
                    else
                        let var = parseVar("tru")
                        TVar(var)
                else
                    let var = parseVar("tr")
                    TVar(var)
            | _ -> let var = parseVar("t")
                   TVar(var)
          | 'n' ->
            eatChar();
            match char(s.Peek()) with
            | 'o' ->
                eatChar();
                if char(s.Peek()) = 't' then
                    eatChar(); TNot
                else
                    let var = parseVar("no")
                    TVar(var)
            | _ -> let var = parseVar("n")
                   TVar(var)
          | 'e' ->
            eatChar();
            match char(s.Peek()) with
            | 'l' ->
                eatChar();
                if char(s.Peek()) = 's' then
                    eatChar();
                    if char(s.Peek()) = 'e' then
                        eatChar(); Else
                    else
                        let var = parseVar("els")
                        TVar(var)
                else
                    let var = parseVar("el")
                    TVar(var)
            | 'n' ->
                eatChar();
                if char(s.Peek()) = 'd' then
                    eatChar(); End
                else
                    let var = parseVar("en")
                    TVar(var)
            | _ -> let var = parseVar("e")
                   TVar(var)
          | 'f' ->
            eatChar();
            match char(s.Peek()) with
            | 'a' ->
                eatChar();
                if char(s.Peek()) = 'l' then
                    eatChar();
                    if char(s.Peek()) = 's' then
                        eatChar();
                        if char(s.Peek()) = 'e' then
                            eatChar(); TBoolean(false)
                        else
                            let var = parseVar("fals")
                            TVar(var)
                    else
                        let var = parseVar("fal")
                        TVar(var)
                else
                    let var = parseVar("fa")
                    TVar(var)
            | _ -> let var = parseVar("f")
                   if char(s.Peek()) = '=' then
                        TVar(var)
                    else
                        failwith "Syntax Error"
          | _ -> 
            if Regex("[^efilnt]+").Match("" + char(c).ToString()).Success then
                let var = parseVar("")
                let z = char(s.Peek())
                if char(s.Peek()) = '=' then
                        TVar(var)
                    else
                        failwith "Syntax Error"
            else
                failwith "Unexpected character"
        
    lastToken

type Ide = string 

// Recursive descent parsing strategy
type Expr =
| Number of int
| Boolean of bool
| Sum of Expr * Expr
| Sub of Expr * Expr
| Quoz of Expr * Expr
| Prod of Expr * Expr
| Greater of Expr * Expr
| Lower of Expr * Expr
| Different of Expr * Expr
| Equal of Expr * Expr
| And of Expr * Expr
| Or of Expr * Expr
| Not of Expr
| Name of Ide
| IfThenElse of Expr * Expr * Expr
| Let of Expr * Expr * Expr
| Epsilon

type Com =
| CIfThenElse of Expr * Com list * Com list

type Prog =
| Dec of Com
| Eval of Expr

type Parser() =

  member this.parseC(t:Tokenizer) : Expr =
    if t.CurrentToken = TLet then
        t.NextToken() |> ignore
        let ide = this.parseTerm(t)
        t.NextToken() |> ignore
        if t.CurrentToken <> TEqual then failwith "Syntax Error"
        
        t.NextToken() |> ignore
        let e1 = this.parseE(t)
        if t.CurrentToken <> TIn || e1 = Epsilon then failwith "Syntax Error"
        t.NextToken() |> ignore
        let e2 = this.parseE(t)
        if e2 = Epsilon then failwith "Syntax Error"
        Let(ide, e1, e2)
    else
        Epsilon 
  
  member this.parseTerm (t:Tokenizer) : Expr =
    match t.CurrentToken with
    | OpenBrace ->
        t.NextToken() |> ignore
        let e = this.parseE(t)
        if t.CurrentToken <> ClosedBrace then
            failwith "Syntax error"
        else
            e
    | Num(n) -> Number(n)
    | TBoolean(true) -> Boolean(true)
    | TBoolean(false) -> Boolean(false)
    | TNot -> 
        t.NextToken() |> ignore
        let e = this.parseE(t)
        Not(e)
    | If -> 
        t.NextToken() |> ignore
        let guard = this.parseE(t)
        if t.CurrentToken = Then then
            t.NextToken() |> ignore
            let the = this.parseE(t)
            if t.CurrentToken = Else then
                t.NextToken() |> ignore
                let els = this.parseE(t)
                if t.CurrentToken <> End then
                    failwith "Syntax Error"
                else
                    IfThenElse(guard, the, els)
            else
                failwith "Syntax Error"
        else
            failwith "Syntax Error"
    | TVar(s) -> Name(s)     
    | _ -> failwith "Syntax Error"
 
  member this.parseP1(t:Tokenizer) : Expr =
    match t.CurrentToken with
    | Mul ->
        t.NextToken() |> ignore
        let p = this.parseP(t)
        Prod(Epsilon, p) 
    | Div ->
        t.NextToken() |> ignore
        let p = this.parseP(t)
        Quoz(Epsilon, p) 
    | _ -> Epsilon
    
  member this.parseP(t :Tokenizer) : Expr =
    let term = this.parseTerm(t)
    t.NextToken() |> ignore
    let p = this.parseP1(t)
    if p = Epsilon then
        let b = this.parseB(t)
        match b with 
        | Greater(Epsilon,e2) -> Greater(term, e2)
        | Lower(Epsilon, e2) -> Lower(term, e2)
        | Equal(Epsilon, e2) -> Equal(term, e2)
        | And(Epsilon, e2) -> And(term, e2)
        | Or(Epsilon, e2) -> Or(term, e2)
        | Different(Epsilon, e2) -> Different(term, e2)
        | _ -> term
    else
        match p with
        |Prod(Epsilon, e1) -> Prod(term, e1)
        |Quoz(Epsilon, e1) -> Quoz(term, e1)
        | _ -> failwith "Syntax Error"


  member this.parseB (t:Tokenizer) : Expr =
    match t.CurrentToken with 
    | TGreater -> t.NextToken() |> ignore
                  let e1 = this.parseE(t)
                  Greater(Epsilon, e1)
    | TLower -> t.NextToken() |> ignore
                let e1 = this.parseE(t)
                Lower(Epsilon, e1)
    | TEqual -> t.NextToken() |> ignore
                let e1 = this.parseE(t)
                Equal(Epsilon, e1)
    | TDifferent -> t.NextToken() |> ignore
                    let e1 = this.parseE(t)
                    Different(Epsilon, e1)
    | TAnd -> t.NextToken() |> ignore
              let e1 = this.parseE(t)
              And(Epsilon, e1)
    | TOr -> t.NextToken() |> ignore
             let e1 = this.parseE(t)
             Or(Epsilon, e1)
    | _ -> Epsilon
        
  member this.parseE1 (t:Tokenizer) : Expr =
    if t.CurrentToken = Plus then
      t.NextToken() |> ignore
      let p = this.parseP(t)
      let e1 = this.parseE1(t)
      match e1 with
       | Sum(Epsilon,ee2) -> Sum(Epsilon, Sum(p,ee2))
       | Sub(Epsilon, ee2) -> Sum(Epsilon, Sub(p,ee2))
       | Epsilon -> Sum(Epsilon, p)
       | _ -> failwith "Syntax Error"
    else
        if t.CurrentToken = Minus then
            t.NextToken() |> ignore
            let p = this.parseP(t)
            t.NextToken() |> ignore
            let e1 = this.parseE1(t)
            match e1 with
            | Sum(Epsilon,ee2) -> Sub(Epsilon, Sum(p,ee2))
            | Sub(Epsilon, ee2) -> Sub(Epsilon, Sub(p,ee2))
            | Epsilon -> Sub(Epsilon, p)
            | _ -> failwith "Syntax Error"
        else
            Epsilon
  
  member this.parseE (t:Tokenizer) : Expr =
    if t.CurrentToken = BOF then t.NextToken() |> ignore
    let c = this.parseC(t)
    if c = Epsilon then
        let p = this.parseP(t)
        //t.NextToken() |> ignore
        let e1 = this.parseE1(t)
        if p <> Epsilon then
            if e1 = Epsilon then
                p
            else
                match e1 with
                | Sum(Epsilon,e2) -> Sum(p, e2)
                | Sub(Epsilon,e2) -> Sub(p, e2)
                | _ -> failwith "Syntax Error"
        else
            match e1 with
            | Epsilon -> Epsilon
            | _ -> e1
    else
        c

[<EntryPoint>]
let main argv = 
    
    let rec interpret (e:Expr) : eval =
        match e with
        | Sum(s1, s2) -> 
            let a1 = interpret(s1)
            let a2 = interpret(s2)
            match (a1,a2) with
            | (Int(n1), Int(n2)) -> Int(n1 + n2)
            | _ -> failwith "Type Mismatch"
        | Sub(s1, s2) -> 
            let a1 = interpret(s1)
            let a2 = interpret(s2)
            match (a1,a2) with
            | (Int(n1), Int(n2)) -> Int(n1 - n2)
            | _ -> failwith "Type Mismatch"
        | Quoz(s1, s2) -> 
            let a2 = interpret(s2)
            let a1 = interpret(s1)
            if a2 <> Int(0) then
                match (a1,a2) with
                | (Int(n1), Int(n2)) -> Int(n1 / n2)
                | _ -> failwith "Type Mismatch"
            else
                failwith "Division by 0" 
        | Prod(p1, p2) -> 
            let a1 = interpret(p1) 
            let a2 = interpret(p2)
            match (a1,a2) with
            | (Int(n1), Int(n2)) -> Int(n1 * n2)
            | _ -> failwith "Type Mismatch"
        | Greater(e1, e2) -> 
            let a1 = interpret(e1)
            let a2 = interpret(e2)
            match(a1,a2) with
            | (Int(b1), Int(b2)) -> Bool(b1 > b2)
            | _ -> failwith "Type Mismatch"            
        | Lower(e1, e2) -> 
            let a1 = interpret(e1)
            let a2 = interpret(e2)
            match(a1,a2) with
            | (Int(b1), Int(b2)) -> Bool(b1 < b2)
            | _ -> failwith "Type Mismatch"
        | Different(e1, e2) ->
            let a1 = interpret(e1)
            let a2 = interpret(e2)
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 <> b2)
            | (Int(n1), Int(n2)) -> Bool(n1 <> n2)
            | _ -> failwith "Type Mismatch"
        | Equal(e1, e2) -> 
            let a1 = interpret(e1)
            let a2 = interpret(e2)
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 = b2)
            | (Int(n1), Int(n2)) -> Bool(n1 = n2)
            | _ -> failwith "Type Mismatch"
        | And(e1, e2) -> 
            let a1 = interpret(e1)
            let a2 = interpret(e2)
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 && b2)
            | _ -> failwith "Type Mismatch"
        | Or(e1, e2) -> 
            let a1 = interpret(e1)
            let a2 = interpret(e2)
            match(a1,a2) with
            | (Bool(b1), Bool(b2)) -> Bool(b1 || b2)
            | _ -> failwith "Type Mismatch"
        | Not(e1) -> 
            let a1 = interpret(e1)
            match a1 with
            | Bool(b1) -> Bool(not b1)
            | Int(n1) -> Int(-n1)
            | _ -> failwith "Type Mismatch"
        | IfThenElse(g, e1, e2) ->
            let gres = interpret(g)
            match gres with
            | Bool(b1) -> 
                if b1 = true then
                    interpret(e1)
                else
                    interpret(e2)
            | _ -> failwith "Type Mismatch"
        | Let(Name(s), e1, e2) -> 
            let ie1 = interpret(e1) in
            Env.extend(s, ie1)
            interpret(e2)
        | Name(s) -> Env.lookup (s) (Env.getInternalEnv())
        | Boolean(true) -> Bool(true)
        | Boolean(false) -> Bool(false)
        | Number(n) -> Int(n)
        | Epsilon -> Unbound
        | _ -> failwith "Internal error"
 
    let ms = new MemoryStream()
    let w = new StreamWriter(ms)
    w.Write("if (+3 < +4) && (+5 < +7) then +2 else -2 end")
    w.Flush()

    ms.Position <- int64(0)
    let s = new StreamReader(ms)
    let t = new Tokenizer(s)
    let p = new Parser()
    let e = p.parseE(t)
 
    printfn "result = %A" (interpret(e))
    0