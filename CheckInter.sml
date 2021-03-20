use "Plc.sml";

let 
    val test = eval (fromString "3::7::t") [("t", IntV 19)]
in
    print("ERROR: Impossible exception should have been raised.\n")
end handle Impossible => print ("INFO: Expected exception. Can't use :: without a list as initial element.\n");

let 
    val test = eval (fromString "hd ([Int] [])") []
in
    print("ERROR: HDEmptySeq exception should have been raised.\n")
end handle HDEmptySeq => print ("INFO: Expected exception. Trying to access head of an empty sequence.\n");

let 
    val test = eval (fromString "tl ([Int] [])") []
in
    print("ERROR: TLEmptySeq exception should have been raised.\n")
end handle TLEmptySeq => print ("INFO: Expected exception. Trying to access tail of an empty sequence.\n");

let 
    val test = eval (fromString "var x = 3; x(1)") []
in
    print("ERROR: NotAFunc exception should have been raised.\n")
end handle NotAFunc => print ("INFO: Expected exception. Variable that is not a function is being called.\n");

let 
    val test = eval (fromString "match x with | 0 -> 1 end") [("x", IntV 3)]
in
    print("ERROR: ValueNotFoundInMatch exception should have been raised.\n")
end handle ValueNotFoundInMatch => print ("INFO: Expected exception. Could not find value in match.\n");

print("INFO: Interpreter testing complete!\n")