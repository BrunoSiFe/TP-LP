(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run exp =
  let
    val expType = let in
      teval exp []
    end
    handle SymbolNotFound => let val p = print ("Error: Expression contains unrecognized symbols.") in raise SymbolNotFound end
          | EmptySeq => let val p =  print ("Error: Sequence without type.") in raise EmptySeq end
          | NotEqTypes => let val p =  print ("Error: Comparisson between two different data types.") in raise NotEqTypes end
          | WrongRetType => let val p =  print ("Error: Function body  does not match return type.") in raise WrongRetType end
          | DiffBrTypes => let val p =  print ("Error: Different branch types.") in raise DiffBrTypes end
          | IfCondNotBool => let val p =  print ("Error: Not boolean condition.") in raise IfCondNotBool end
          | NoMatchResults => let val p =  print ("Error: There is no match pattern defined.") in raise NoMatchResults end
          | MatchResTypeDiff => let val p =  print ("Error: Match results do not match.") in raise MatchResTypeDiff end
          | MatchCondTypesDiff => let val p =  print ("Error: Match expression does not match with the conditions type.") in raise MatchCondTypesDiff end
          | CallTypeMisM => let val p =  print ("Error: Formal arguments does not match the function argument type.") in raise CallTypeMisM end
          | NotFunc => let val p =  print ("Error: Variable that is not a function is being called.") in raise NotFunc end
          | ListOutOfRange => let val p =  print ("Error: Array out of bounds.") in raise ListOutOfRange end
          | OpNonList => let val p =  print ("Error: Can not access element in not a list.") in raise OpNonList end
          | UnknownType => let val p = print ("Error: Unknown type used.") in raise UnknownType end
          | _ => let val p = print ("Error: Exception Raised.") in raise UnknownType end
    val expResult = let in
      eval exp []
    end
    handle HDEmptySeq => let val p =  print ("Error: Can not acces head of empty sequence.") in raise HDEmptySeq end
         | TLEmptySeq => let val p =  print ("Error: Can not acces tail of empty sequence.") in raise TLEmptySeq end
         | ValueNotFoundInMatch => let val p =  print ("Error: Could not match with the match list.") in raise ValueNotFoundInMatch end
         | Impossible => let val p = print ("Error: Impossible action.") in raise Impossible end
         | _ => let val p = print ("Error: Exception Raised.") in raise Impossible end
  in
    val2string(expResult) ^ " : " ^ type2string(expType)
  end