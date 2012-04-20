namespace Interpreter 

module Tests =

    open System
    open NUnit.Framework
    open Interpreter
    
    let complex1 = eval(If(Const 7,Const 3,Const 4))
    let complexResult1 = Const 3
    
    let complex2 = eval(Div(Minus(Plus(Const 1, Const 2),Times(Const 3,Const 4)),Const 5))
    let complexResult2 = Const -1

    let complex3 = eval (If(Fun("x",Var "x"),Const 3,Const 4))
    let complexResult3 = Const 3

    let complex4 = eval(If(Const 0,Const 3,Const 4))
    let complexResult4 = Const 4
    //let complex4 = App(App(Y, Fun("f",Fun("n",If(Var "n",Times(Var "n",App(Var("f"),Minus(Var "n",Const 1))),Const 1)))), Const 5)
    let complex5 = eval(Fun("x",Plus(Const 7,Var("x"))))
    let complexResult5 = Fun("x",Plus(Const 7,Var("x")))
    
    let complex6 = eval(App(Fun("x",Plus(Const 7,Var("x"))),Const 3))
    let complexResult6 = Const 10

    let complex7 = eval (If(Fun("x",Var "y"),Const 3,Const 4))
    let complexResult7 = Const 3

    let hellWorld = App(mkFac, Const 5)
    let complex8 = App(hellWorld, Const 5)
    let complexResult8 = Const 120
    
    //printfn "%A" complex2
    //Console.ReadKey();

    [<TestFixture>] 
    type ProjectTests() = 
        
        [<Test>] member test.
         ``ComplexGroup1`` () =
            Assert.AreEqual(complexResult1, complex1)
        
        [<Test>] member test.
         ``ComplexGroup2`` () =
            Assert.AreEqual(complexResult2, complex2)

        [<Test>] member test.
         ``ComplexGroup3`` () =
            Assert.AreEqual(complexResult3, complex3)
        
        [<Test>] member test.
         ``ComplexGroup4`` () =
            Assert.AreEqual(complexResult4, complex4)

        [<Test>] member test.
         ``ComplexGroup5`` () =
            Assert.AreEqual(complexResult5, complex5)

        [<Test>] member test.
         ``ComplexGroup6`` () =
            Assert.AreEqual(complexResult6, complex6)

        [<Test>] member test.
         ``ComplexGroup7`` () =
            Assert.AreEqual(complexResult7, complex7)
                
        [<Test>] member test.
         ``ComplexGroup8`` () =
            Assert.AreEqual(complexResult8, complex8)       

    

