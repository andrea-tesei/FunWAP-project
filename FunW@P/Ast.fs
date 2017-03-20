namespace AP

[<AutoOpen>]
module Ast =

    type Ide = string

    // Return types for variables
    type Type = 
    | TBool 
    | TInt 
    | TFun 
    
    // Return types for functions
    and RType = 
    | FBool
    | FInt
    | FFun of Type list * RType

    type Param = Param of string * Type

    // Declarations
    type Decl =
    | DecAssign of Ide * Type * Expr
    | Dec of Ide * Type

    // Statements
    and Stmt = 
    | Assign of Ide * Expr
    | IfThenElse of Expr * Block * Block
    | IfThen of Expr * Block
    | For of ForStmt
    | Println of Expr
    
    // For statement
    and ForStmt = {
        Init: Stmt
        Cond: Expr
        Update: Stmt
        Block: Block
    }

    // Block definition
    and Block = Block of Decl list * Stmt list

    // Return types for a function
    and RetFun = 
    | RetExpr of Expr
    | AFun of AnonymousFunDec

    // Function declaration
    and Fundecl = 
    | Fun of FunDec
    
    // Function declaration
    and FunDec = { 
        Name: string
        Params: Param list
        TypeRet: RType
        Block: Block
        Ret: RetFun
    }

    // Anonymous function declaration
    and AnonymousFunDec = { 
        Params: Param list
        TypeRet: RType
        Block: Block
        Ret: RetFun
    }

    // Expressions
    and Expr =
    | Or of Expr * Expr
    | And of Expr * Expr
    | Equal of Expr * Expr
    | Differ of Expr * Expr
    | Lower of Expr * Expr
    | Greater of Expr * Expr
    | LowerEQ of Expr * Expr
    | GreaterEQ of Expr * Expr
    | Sum of Expr * Expr
    | Sub of Expr * Expr
    | Prod of Expr * Expr
    | Div of Expr * Expr
    | Not of Expr
    | Minus of Expr
    | Num of int
    | Boolean of bool
    | Var of Ide
    | Call of Ide * Expr list
    | Async of Expr
    | DAsync of Ide * Expr
    | ParExp of Expr

    // Program definition
    type Prog = Prog of ProgSignature
    
    // Program signature
    and ProgSignature = { 
        Fundecs: Fundecl list
        Main: Block
    }