use "Plc.sml";

teval(If(Prim2("=", ConI 11, ConI 12), ConI 1, ConI 0))[];

teval(Let("b", Prim2("=", ConI 1, ConI 2), If(Var "b", ConI 3, ConI 4)))[];

teval(Letrec("f1",IntT,"x",IntT,Prim2 ("+",Var "x",ConI 1),Call (Var "f1",ConI 12)))[];

(*Cases given by the professor that raise expections.*)

(*teval(Let("b", Prim2("=", ConI 1, ConI 2),If(Var "b", Var "b", ConI 6)))[];

teval(Let("f",Anon (BoolT,"x",If (Var "x",ConI 11,ConI 22)),Call (Var "f",ConI 0)))[];

teval(Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true)))[];*)

let
    val test = teval (fromString "(Bool [])") [];
in
    print("Error: EmptySeq excpetionn expected.\n")
end handle EmptySeq => print ("Info: Sequences must declare their sequence type like [plcType].\n");

let
    val test = teval (fromString "1::2::tail") [("tail", IntT)]
in
    print("Error: UnknownType exception expected.\n")
end handle UnknownType => print ("Info: Can't use :: without a list as initial element.\n");

let
    val test = teval (fromString "if 1 != false then 0 else m; f(5, 8)") [];
in
    print("Error: NotEqTypes excpetionn expected.\n")
end handle NotEqTypes => print ("Info: Trying to compare different types.\n");

let
    val test = teval (fromString "fun rec f(Int a, Int b):Int = if a != 0 then true else false; f(1, 2)") [];
in
    print("Error: WrongRetType excpetionn expected.\n")
end handle WrongRetType => print ("Info: Function body does not match return type.\n");

let
    val test = teval (fromString "if true then 10 else false") []
in
    print("Error: DiffBrTypes excpetionn expected.\n")
end handle DiffBrTypes => print ("Info: Branches have different types.\n");

let
    val test = teval (fromString "if 10 then 1 else 0") []
in
    print("Error: IfCondNotBool excpetionn expected.\n")
end handle IfCondNotBool => print ("Info: Condition evaluetade is not a bool.\n");

let
    val test = teval (fromString "match case with end") [("case", IntT)]
in
    print("Error: NoMatchResults excpetionn expected.\n")
end handle NoMatchResults => print ("Info: There is no match pattern defined.\n");

let
    val test = teval (fromString "match case with | 0 -> false | _ -> 1 end") [("case", IntT)]
in
    print("Error: MatchResTypeDiff excpetionn expected.\n")
end handle MatchResTypeDiff => print ("Info: Match result different.\n");

let
    val test = teval (fromString "match case with | true -> 1 | false -> 0 end") [("case", IntT)]
in
    print("Error: MatchCondTypesDiff excpetionn expected.\n")
end handle MatchCondTypesDiff => print ("Info: Trying to match different types.\n");

let
    val test = teval (fromString "fun f(Bool a, Int b) = if a = true then 0 else b; f(1, 2)") [];
in
    print("Error: CallTypeMisM excpetionn expected.\n")
end handle CallTypeMisM => print ("Info: Formal arguments does not match the function argument type.\n");

let
    val test = teval (fromString "var variable = 1; variable(5)") []
in
    print("Error: NotFunc excpetionn expected.\n")
end handle NotFunc => print ("Info: Variable that is not a function is being called.\n");

let
    val test = teval (fromString "(1,2,true,false)[10]") []
in
    print("Error: ListOutOfRange excpetionn expected.\n")
end handle ListOutOfRange => print ("Info: Array out of bounds.\n");

let
    val test = teval (fromString "var variable = 1; variable[1]") []
in
    print("Error: OpNonList excpetionn expected.\n")
end handle OpNonList => print ("Info: Using indexes in not a list.\n");

print("Info: Tests Completed.\n")