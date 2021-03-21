use "Plc.sml";

let 
    val test = eval (fromString "1::2::tail") [("tail", IntV 1)]
in
    print("Error: Impossible exception expected.\n")
end handle Impossible => print ("Info: Can't use :: without a list as initial element.\n");

let 
    val test = eval (fromString "hd ([Int] [])") []
in
    print("Error: HDEmptySeq exception expected.\n")
end handle HDEmptySeq => print ("Info: Trying to access head of an empty sequence.\n");

let 
    val test = eval (fromString "tl ([Int] [])") []
in
    print("ERROR: TLEmptySeq expected.\n")
end handle TLEmptySeq => print ("Info: Trying to access tail of an empty sequence.\n");

let 
    val test = eval (fromString "match case with | 0 -> 1 end") [("case", IntV 1)]
in
    print("ERROR: ValueNotFoundInMatch expected.\n")
end handle ValueNotFoundInMatch => print ("Info: Could not find value in match.\n");

let 
    val test = eval (fromString "var variable = 1; variable(true)") []
in
    print("ERROR: NotAFunc expected.\n")
end handle NotAFunc => print ("Info: Variable that is not a function is being called.\n");

print(" Info: Test Completed\n")