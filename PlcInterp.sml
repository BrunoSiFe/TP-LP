(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (ConI i) _ = IntV i
  | eval (ConB b) _ = BoolV b
  | eval (ESeq e) _ = SeqV []
  | eval (Var v) (env:plcVal env) = let in lookup env v end
  | eval (Item (index, exp)) (env:plcVal env) =
    let
      fun getElementList (index, []) = raise Impossible
        | getElementList (index, (x::[])) = if index = 1 then x else raise Impossible
        | getElementList (index, (x::xs)) = if index = 1 then x else getElementList (index - 1, xs)
      val value = eval exp env
    in
      case value of
          ListV l => getElementList (index, l)
        | SeqV s => getElementList (index, s)
        | _ => raise Impossible
    end
  | eval (List []) (env:plcVal env) = ListV []
  | eval (List l) (env:plcVal env) = 
    let
      fun unroll (x::[]) = eval x env :: []
        | unroll (x::xs) = eval x env :: unroll xs
        | unroll _ = raise Impossible;
    in
      ListV (unroll l)
    end
  | eval (If (firstExp, secondExp, thirdExp)) (env:plcVal env) = 
    let in
      case eval firstExp env of 
          BoolV true => eval secondExp env
        | BoolV false => eval thirdExp env
        | _ => raise Impossible
    end
  | eval (Match (firstExp, matchList)) (env:plcVal env) = 
    let 
      val evalMatchVar = eval firstExp env 
      fun tryMatches (matchVar, x::[]) env =
          let in
            case x of
                (SOME secondExp, thirdExp) => if matchVar = eval secondExp env then thirdExp else raise ValueNotFoundInMatch
              | (NONE, thirdExp) => thirdExp
          end
        | tryMatches (matchVar, x::xs) env =  let in
            case x of
                (SOME secondExp, thirdExp) => if matchVar = eval secondExp env then thirdExp else tryMatches (matchVar, xs) env
              | (NONE, thirdExp) => raise Impossible
          end
        | tryMatches (matchVar, _ ) env = raise Impossible
    in
      eval (tryMatches (evalMatchVar, matchList) env) env
    end
  | eval (Prim1 (operation, exp)) (env:plcVal env) =
    let
      val v = eval exp env
    in
      case v of
          IntV i => 
          let in
            case operation of
                "-" => IntV (~ i)
              | "print" => 
                let 
                  val v = IntV i
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | BoolV b =>
          let in
            case operation of
                "!" => BoolV (not b)
              | "print" => 
                let 
                  val v = BoolV b
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | SeqV s =>
          let in
            case operation of
                "hd" => let in let in hd s end handle Empty => raise HDEmptySeq end
              | "tl" => let in let in SeqV (tl s) end handle Empty => raise TLEmptySeq end
              | "ise" =>
                let in
                  case s of
                      [] => BoolV true
                    | _ => BoolV false
                end
              | "print" => 
                let 
                  val ignore = print(list2string(val2string, s) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | ListV l =>
          let in
            case operation of
                "print" => 
                let 
                  val ignore = print(list2string(val2string, l) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | _ => raise Impossible
    end
  | eval (Prim2 (operation, firstExp, secondExp)) (env:plcVal env) =
    if operation = ";" then
      let
        val ignore = eval firstExp env
      in
        eval secondExp env
      end
    else
      let
        val v1 = eval firstExp env
        val v2 = eval secondExp env
      in
        case (v1, v2) of
            (IntV i1, IntV i2) => 
            let in
              case operation of
                  "+" => IntV (i1 + i2)
                | "-" => IntV (i1 - i2)
                | "*" => IntV (i1 * i2)
                | "/" => IntV (i1 div i2)
                | "<" => BoolV (i1 < i2)
                | "<=" => BoolV (i1 <= i2)
                | "=" => BoolV (i1 = i2)
                | "!=" => BoolV (i1 <> i2)
                | _ => raise Impossible
            end
          | (BoolV b1, BoolV b2) => 
            let in
              case operation of
                  "&&" => BoolV (b1 andalso b2)
                | "=" => BoolV (b1 = b2)
                | "!=" => BoolV (b1 <> b2)
                | _ => raise Impossible
            end
          | (IntV i1, SeqV s2) => 
            let in
              case operation of
                  "::" => SeqV (IntV i1 :: s2)
                | _ => raise Impossible
            end
          | (BoolV b1, SeqV s2) => 
            let in
              case operation of
                  "::" => SeqV (BoolV b1 :: s2)
                | _ => raise Impossible
            end
          | (ListV l1, SeqV s2) => 
            let in
              case operation of
                  "::" => SeqV (ListV l1 :: s2)
                | _ => raise Impossible
            end
          | _ => raise Impossible
      end
  | eval (Let (var, firstExp, secondExp)) (env:plcVal env) =
    let
      val nEnv = (var, eval firstExp env) :: env
    in
      eval secondExp nEnv
    end
  | eval (Anon (typ, argument, exp)) (env:plcVal env) = Clos ("", argument, exp, env)
  | eval (Call (firstExp, secondExp)) (env:plcVal env) = 
    let
      fun mountArguments (List (x::[])) = [eval x env]
        | mountArguments (List (x::xs)) = [eval x env] @ mountArguments (List xs)
        | mountArguments (exp) = [eval exp env]
      val nEnv = [("$list", ListV (mountArguments secondExp))] @ env
      val f = eval firstExp env
    in
      case f of
          Clos(name, var, exp, cEnv) =>
            let
              val ev = eval secondExp nEnv
              val fEnv = (var, ev)::(name, f)::cEnv
            in
              eval exp fEnv
            end
        | _ => raise NotAFunc
    end
  | eval (Letrec (functionName, argumentType, argument, functionType, firstExp, secondExp)) (env:plcVal env) =
    let
      val nEnv = (functionName, Clos(functionName, argument, firstExp, env)) :: env
    in
      eval secondExp nEnv
    end
  ;