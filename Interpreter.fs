module Interpreter
open System
//Abstract Syntax Tree is defined as follows
type Expr =
    | Const of int
    | Bool of bool
    | Var of string
    | Fun of string * Expr
    | Plus of Expr * Expr
    | Times of Expr * Expr
    | Minus of Expr * Expr
    | Div of Expr * Expr
    | If of Expr * Expr * Expr
    | App of Expr * Expr
    | Y
    

//our subtitute function
//Idea was acquired from the ML Interpreter
let rec subst (x:String, v:Expr, a:Expr):Expr = match a with
    | Const _ -> a
    | Var y -> if x = y then v else a
    | Plus (x', y') -> Plus(subst(x, v, x'), subst(x ,v , y'))
    | Minus(x', y') -> Minus(subst(x, v, x'), subst(x, v, y'))
    | Times(x', y') -> Times(subst(x, v, x'), subst(x, v, y'))
    | Div(x', y') -> Div(subst(x, v, x'), subst(x, v, y'))
    | If(x', y', z') -> If(subst(x, v, x'), subst(x, v, y'), subst(x, v, z'))      
    | Fun(y', a') -> if x = y' then a else Fun(y', subst (x, v, a'))
    | App (x', y') -> App (subst(x, v, x'), subst(x, v, y'))
    //| Y(x') -> Y(subst(x, v, x')) //Not sure this would be needed.
    
let rec eval(e: Expr) = match e with
        | Const c -> Const c
        | Bool(b) -> Bool(b)
        | Plus(e1, e2) -> match  (eval e1, eval e2) with
           | (Const x, Const y) -> Const(x + y)
           | _ -> Plus(eval (e1), eval(e2))
        | Minus(e1, e2) -> match (eval e1, eval e2) with
            | (Const x, Const y) -> Const(x - y)
            | _ -> Minus(eval (e1), eval(e2))
        | Times(e1, e2) -> match (eval e1, eval e2) with
            | (Const x, Const y) -> Const(x * y)
            | _ -> Times(eval (e1), eval(e2))
        | Div(e1, e2) -> match(eval e1, eval e2) with
            | (Const x, Const y) -> Const(x / y)
            | _ -> Times(eval (e1), eval(e2))
        | Var x -> failwith "error (* your eval function should no longer include a branch for Var  *)"
        | If (c, e1, e2) -> if eval c = Const 0 then eval e2 else eval e1
        | Fun(x, e1) -> Fun(x, e1)
        | App(e1, e2) -> match eval e1 with 
            | Fun(x, e) -> eval(subst (x, e2, e))
            | Var _ -> e1
        //Y = lambda G. (lambda g. G(lambda x. g g x)) (lambda g. G(lambda x. g g x)) // Crazy expression to create here //
        | Y -> Fun("G", App(Fun("g", App(Var("G"), App(Var("g"), Var("g")))), Fun("g", App(Var("G"), App(Var("g"), Var("g"))))))
        | _ -> failwith "I could not match the patterns"

(* Since this expression causes and error, I am disabling this
let e1 = Var "x"
printfn "%A" (eval e1)
*)
let e2 = Fun("x",Plus(Const 7,Var("x")))
printfn "%A" (eval e2)

let e3 = App(Fun("x",Plus(Const 7,Var("x"))),Const 3)
printfn "%A" (eval e3)

let e8 = eval (If(Const 7,Const 3,Const 4))
printfn "%A" (eval e8)

let e4 = If(Const 0,Const 3,Const 4)
printfn "%A" (eval e4)

let e5 = If(Fun("x",Var "x"),Const 3,Const 4)
printfn "%A" (eval e5)

let e6 = If(Fun("x",Var "y"),Const 3,Const 4)
printfn "%A" (eval e6)
        
let e7 = App(App(Y, Fun("f",Fun("n",If(Var "n",Times(Var "n",App(Var("f"),Minus(Var "n",Const 1))),Const 1)))), Const 5)
printfn "%A" (eval e7) //Yahoo!! This printed Const 120 eventually. More on UnitTests
Console.ReadKey()

// eval (App(Var("x"),Const 3) = this expression does not even evalute or pass through the IDE, so no need to evaluate
(*
It seems to be that Visual Studio 2010 and nUnit, at least as far as F# is concerned has some weird problems. Everytime I include the file, this console stops printing.
Although a separate unit test is written, I have excluded the unit test from my project so that I can evaluate and check the results in the console.
Test Prints results
*)

(*Dr. Laufer's expressions
eval (Var "x") -> error (* your eval function should no longer include a branch for Var  *)
eval (Fun("x",Plus(Const 7,Var("x")))) -> Fun("x",Plus(Const 7,Var("x")))
eval (App(Fun("x",Plus(Const 7,Var("x"))),Const 3)) -> Const 10
eval (App(Var("x"),Const 3) -> error
eval (If(Const 7,Const 3,Const 4)) -> Const 3
eval (If(Const 0,Const 3,Const 4)) -> Const 4
eval (If(Fun("x",Var "x"),Const 3,Const 4)) -> Const 3
eval (If(Fun("x",Var "y"),Const 3,Const 4)) -> Const 3
let Y = (* you get to define it yourself, as in the handout -- this is cool! *)
eval (App(App(Y, Fun("f",Fun("n",If(Var "n",Times(Var "n",App(Var("f"),Minus(Var "n",Const 1))),Const 1)))), Const 5) -> Const 120
*)



