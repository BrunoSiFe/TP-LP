use "Plc.sml";

let
    val test = teval (fromString "3::7::t") [("t", IntT)]
in
    print("ERROR: UnknownType exception should have been raised.\n")
end handle UnknownType => print ("INFO: Expected exception. Can't use :: without a list as initial element.\n");

let
    val test = teval (fromString "(Int [])") [];
in
    print("ERROR: EmptySeq exception should have been raised.\n")
end handle EmptySeq => print ("INFO: Expected exception. Sequences must declare their sequence type like [plcType].\n");

let
    val test = teval (fromString "match x with | 0 -> true | _ -> 1 end") [("x", IntT)]
in
    print("ERROR: MatchResTypeDiff exception should have been raised.\n")
end handle MatchResTypeDiff => print ("INFO: Expected exception. Match result types are different.\n");

let
    val test = teval (fromString "match x with | true -> 1| _ -> -1 end") [("x", IntT)]
in
    print("ERROR: MatchCondTypesDiff exception should have been raised.\n")
end handle MatchCondTypesDiff => print ("INFO: Expected exception. Match conditions types are different.\n");

let
    val test = teval (fromString "if 5 then 8 else 3") []
in
    print("ERROR: IfCondNotBool exception should have been raised.\n")
end handle IfCondNotBool => print ("INFO: Expected exception. If condition is not bool.\n");

let
    val test = teval (fromString "if true then 8 else false") []
in
    print("ERROR: DiffBrTypes exception should have been raised.\n")
end handle DiffBrTypes => print ("INFO: Expected exception. If branches have different types.\n");

let
    val test = teval (fromString "(6,false)[3]") []
in
    print("ERROR: ListOutOfRange exception should have been raised.\n")
end handle ListOutOfRange => print ("INFO: Expected exception. Trying to access 3rd element from 2 element list.\n");

let
    val test = teval (fromString "if true != 0 then 0 else m; f(5, 8)") [];
in
    print("ERROR: NotEqTypes exception should have been raised.\n")
end handle NotEqTypes => print ("INFO: Expected exception. Comparision done on different types.\n");

let
    val test = teval (fromString "fun rec f(Int n, Int m):Bool = if n != 0 then 0 else m; f(5, 8)") [];
in
    print("ERROR: WrongRetType exception should have been raised.\n")
end handle WrongRetType => print ("INFO: Expected exception. Function return type does not agree with function body.\n");

let
    val test = teval (fromString "fun f(Int n, Int m) = if n != 0 then 0 else m; f(true, 8)") [];
in
    print("ERROR: CallTypeMisM exception should have been raised.\n")
end handle CallTypeMisM => print ("INFO: Expected exception. Function real argument type is different from formal argument types.\n");

let
    val test = teval (fromString "var x = 3; x(1)") []
in
    print("ERROR: NotFunc exception should have been raised.\n")
end handle NotFunc => print ("INFO: Expected exception. Variable that is not a function is being called.\n");

let
    val test = teval (fromString "var x = 3; x[1]") []
in
    print("ERROR: OpNonList exception should have been raised.\n")
end handle OpNonList => print ("INFO: Expected exception. Variable that is not a list is being indexed.\n");

print("INFO: Checker testing complete!\n")