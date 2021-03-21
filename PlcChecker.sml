(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (ConI _) _ = IntT
  | teval (ConB _) _ = BoolT
  | teval (ESeq s) _ =
    let in
      case s of
          SeqT t => SeqT t
        | _ => raise EmptySeq
    end
  | teval (Var v) (env:plcType env) = let in lookup env v end
  | teval (List l) (env:plcType env) =
    let
      fun checkAllElements (x::[]) = (teval x env)::[]
        | checkAllElements (x::xs) = (teval x env)::checkAllElements xs
        | checkAllElements _ = []
      val lst = checkAllElements l
    in
      ListT lst
    end
  | teval (Item (index, exp)) (env:plcType env) =
    let
      fun getListElement (i, []) = raise ListOutOfRange
        | getListElement (i, (x::[])) = if i = 1 then x else raise ListOutOfRange
        | getListElement (i, (x::xs)) = if i = 1 then x else getListElement (i - 1, xs)
      val vType = teval exp env
    in
      case vType of
          ListT l => getListElement(index, l)
        | _ => raise OpNonList
    end
  | teval (If(firstExp, secondExp, thirdExp)) (env:plcType env) =
    let
      val conditionType = teval firstExp env
      val secondExpType = teval secondExp env
      val thirdExpType = teval thirdExp env
    in
      case conditionType of
          BoolT => if secondExpType = thirdExpType then secondExpType else raise DiffBrTypes
        | _ => raise IfCondNotBool
    end
  | teval (Prim1(operation, exp)) (env:plcType env) =
    let
      val expType = teval exp env
    in
      case operation of
          "!" => if expType = BoolT then BoolT else raise UnknownType
        | "-" => if expType = IntT then IntT else raise UnknownType
        | "hd" => let in
            case expType of
                SeqT t => t
              | _ => raise UnknownType
          end
        | "tl" => let in
            case expType of
                SeqT t => SeqT t
              | _ => raise UnknownType
          end
        | "ise" => let in
            case expType of
                SeqT t => BoolT
              | _ => raise UnknownType
          end
        | "print" => ListT []
        | _ => raise UnknownType
    end
  | teval (Prim2(operation, firstExp, secondExp)) (env:plcType env) =
    let
      val firstExpType = teval firstExp env
      val secondExpType = teval secondExp env
    in
      case operation of
          "&&" => if firstExpType = BoolT andalso secondExpType = BoolT then BoolT else raise UnknownType
        | "::" => let in
            case (firstExpType, secondExpType) of
                (IntT, ListT []) => SeqT IntT
              | (IntT, SeqT t2) => if t2 = IntT then SeqT t2 else raise NotEqTypes
              | (BoolT, ListT []) => SeqT BoolT
              | (BoolT, SeqT t2) => if t2 = BoolT then SeqT t2 else raise NotEqTypes
              | (ListT t, ListT []) => SeqT (ListT t)
              | (ListT t, SeqT t2) => if t2 = ListT t then SeqT t2 else raise NotEqTypes
              | _ => raise UnknownType
          end
        | "+" => if firstExpType = IntT andalso secondExpType = IntT then IntT else raise UnknownType
        | "-" => if firstExpType = IntT andalso secondExpType = IntT then IntT else raise UnknownType
        | "*" => if firstExpType = IntT andalso secondExpType = IntT then IntT else raise UnknownType
        | "/" => if firstExpType = IntT andalso secondExpType = IntT then IntT else raise UnknownType
        | "<" => if firstExpType = IntT andalso secondExpType = IntT then BoolT else raise UnknownType
        | "<=" => if firstExpType = IntT andalso secondExpType = IntT then BoolT else raise UnknownType
        | "=" => if firstExpType = secondExpType andalso (firstExpType = IntT orelse firstExpType = BoolT) then BoolT else raise NotEqTypes
        | "!=" => if firstExpType = secondExpType andalso (firstExpType = IntT orelse firstExpType = BoolT) then BoolT else raise NotEqTypes
        | ";" => secondExpType
        | _ => raise UnknownType
    end
  | teval (Let(var, firstExp, secondExp)) (env:plcType env) =
    let
      val firstExpType = teval firstExp env
      val nEnv = (var, firstExpType) :: env
    in
      teval secondExp nEnv
    end
  | teval (Anon(typ, arg, exp)) (env:plcType env) = 
    let
      val nEnv = (arg, typ) :: env
      val expType = teval exp nEnv
    in
      FunT (typ, expType)
    end
  | teval (Call(secondExp, firstExp)) (env:plcType env) =
    let
      val firstExpType = teval firstExp env
      val secondExpType = teval secondExp env
    in
      case secondExpType of
          FunT (argType, resultType) => 
            if firstExpType = argType then resultType else raise CallTypeMisM
        | _ => raise NotFunc
    end
  | teval (Letrec(fName, argTyp, arg, funTyp, firstExp, secondExp)) (env:plcType env) =
    let
      val recEnv = (fName, FunT (argTyp, funTyp))
      val argEnv = (arg, argTyp)
      val firstExpType = teval firstExp (recEnv :: argEnv :: env)
      val secondExpType = teval secondExp (recEnv :: env)
    in
      if firstExpType = funTyp then secondExpType else raise WrongRetType
    end
  | teval (Match(firstExp, matchList)) (env:plcType env) =
    if null matchList then raise NoMatchResults else
      let
        val firstCondition = teval firstExp env
        val firstRes = (#2 (hd matchList))
        val firstResType = teval firstRes env
        fun findMatch (Match(firstExp, matchList)) (env:plcType env) =
            let in
              case matchList of
                  x::[] => let in
                      case x of
                          (SOME secondExp, thirdExp) => 
                            if (teval thirdExp env) = firstResType then
                              if firstCondition = (teval secondExp env) then 
                                teval thirdExp env 
                              else raise MatchCondTypesDiff
                            else raise MatchResTypeDiff
                        | (NONE, thirdExp) => if (teval thirdExp env) = firstResType then firstResType else raise MatchResTypeDiff
                    end
                | x::xs => let in
                      case x of
                          (SOME secondExp, thirdExp) => 
                            if (teval thirdExp env) = firstResType then
                              if firstCondition = (teval secondExp env) then
                                findMatch (Match(firstExp, xs)) env 
                              else raise MatchCondTypesDiff
                            else raise MatchResTypeDiff
                        | _ => raise UnknownType
                    end
            end
          | findMatch _ _ = raise UnknownType
      in
        findMatch (Match(firstExp, matchList)) env
      end