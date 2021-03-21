use "Plc.sml";

let 
    val test = eval (fromString "case") []
in
    print("Error: SymbolNotFound exception expected.\n")
end handle SymbolNotFound => print ("Info: Can't use undefined symbol.\n");

print(" Info: Test Completed\n")