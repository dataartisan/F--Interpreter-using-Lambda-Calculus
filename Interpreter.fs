(*
Author: Subhash P, William E
Purpose: Building a Lambda Interpreter using F#
*)
namespace Interpreter 
module Interpreter =
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
        | Fix of Expr
        
    
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
     
    

   
    // evaluator
    let rec eval(e: Expr) = match e with
        | Const c -> Const c
        | Bool(b) -> Bool(b)
        | Plus(e1, e2) -> match  (eval e1, eval e2) with
           | (Const x, Const y) -> Const(x + y)
           | (Const x, Var y) -> Const x
           | (Var x, Const y) -> Const y
           | _ -> Plus(eval (e1), eval(e2))
        | Minus(e1, e2) -> match (eval e1, eval e2) with
            | (Const x, Const y) -> Const(x - y)
            | (Const x, Var y) -> Const x
            | (Var x, Const y) -> Const y
            | _ -> Minus(eval (e1), eval(e2))
        | Times(e1, e2) -> match (eval e1, eval e2) with
            | (Const x, Const y) -> Const(x * y)
            | (Const x, Var y) -> Const x
            | (Var x, Const y) -> Const y
            | _ -> Times(eval (e1), eval(e2))
        | Div(e1, e2) -> match(eval e1, eval e2) with
            | (Const x, Const y) -> Const(x / y)
            | (Const x, Var y) -> Const x
            | (Var x, Const y) -> Const y
            | _ -> Times(eval (e1), eval(e2))
        | Var x -> failwith "error (* your eval function should no longer include a branch for Var  *)"
        | If (c, e1, e2) -> if eval c = Const 0 then eval e2 else eval e1
        | Fun(x, e1) -> Fun(x, e1)
        | App(f, arguement) -> match eval f with 
            | Fun(x, e) -> eval(subst (x, arguement, e))
            | Var _ -> f
        | _ -> failwith "I could not match the patterns"
        
        
    let rec Fix f x = f (Fix f) x

    (* An usual factorial in F# would look like this
    let regularfactorial fact = function
        0 -> 1
        | x -> x * fact (x-1)
    *)

    //lets attempt to turn it into an expression syntax instead for our purpose

    let mkFac = Fun("f", Fun("n", If(Var "n", Times(Var "n", App(Var "f", Minus(Var "n", Const 1))), Const 1)))
    let a = App(mkFac, mkFac)

      
    let five = Const 5
    let test = App(Fun("x",Plus(Const 7,Var("x"))),Const 3)
    
    
    let hellWorld = App(mkFac, Const 5)
    let Y = App(hellWorld, five)

    
(*
let five = Const(5)
let protofac = Lam("f", Lam("n", IfZero(Var("n"), Const(1), Times(Var("n"), App(App(Var("f"), Var("f")), Minus(Var("n"), Const(1)))))))
let fac = App(protofac, protofac)
let onetwenty = App(fac, five) 
 
eval (Var "x") -> error (* your eval function should no longer include a branch for Var  *) - Our output -> Var "x"
eval (Fun("x",Plus(Const 7,Var("x")))) -> Fun("x",Plus(Const 7,Var("x"))) - Our Output -> (Fun("x",Plus(Const 7,Var("x"))))
eval (App(Fun("x",Plus(Const 7,Var("x"))),Const 3)) -> Const 10 - Our Output -> Problem some exception
eval (App(Var("x"),Const 3) -> error - Our output -> problem
eval (If(Const 7,Const 3,Const 4)) -> Const 3 - Our output -> Const 3
eval (If(Const 0,Const 3,Const 4)) -> Const 4 - Our Output -> Const 4
eval (If(Fun("x",Var "x"),Const 3,Const 4)) -> Const 3 - Our Output -> Const 3
eval (If(Fun("x",Var "y"),Const 3,Const 4)) -> Const 3 - Our output -> Const 3
let Y = (* you get to define it yourself, as in the handout -- this is cool! *) -- Voodooooooo!! I dont know!
eval (App(App(Y, Fun("f",Fun("n",If(Var "n",Times(Var "n",App(Var("f"),Minus(Var "n",Const 1))),Const 1)))), Const 5) -> Const 120 - Y combinator has to be done
*)

