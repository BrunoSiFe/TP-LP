%%

%name PlcParser

%pos int

%term VAR
    | PLUS | MINUS | MULTI | DIV | EQ | NOTEQ
    | LPAR | RPAR | LBRACKET | RBRACKET | LCOL | RCOL
    | LESSER | LESSEREQ | DOUBLEPOINT | SEMIC 
    | IF | THEN | ELSE
    | NOT | NEGATE
    | MATCH | WITH
    | HD | TL | ISE
    | TYPE
    | DPOINT
    | PRINT
    | AND
    | NAME of string | CINT of int | CONBTRUE| CONBFALSE | BOOL of bool
    | FUN | REC 
    | NIL
    | ARROW | FUNARROW
    | COMMA | COLON
    | PIPE | UNDERSCORE
    | END
    | EOF 

%nonterm Prog of expr 
    | Decl of expr | Expr of expr
    | AtomExpr of expr 
    | AppExpr of expr 
    | Const of expr 
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomType of plcType
    | Types of plcType list

%right SEMIC DOUBLEPOINT ARROW
%left  ELSE AND EQ NOTEQ LESSER LESSEREQ PLUS MINUS MULTI DIV LCOL
%nonassoc NOT HD TL ISE PRINT IF NAME

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME,expr,Prog))
    | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN REC NAME Args COLON Type EQ Expr SEMIC Prog (MakeFun(NAME, Args, Type, Expr, Prog))

Expr : AtomExpr(AtomExpr)
    | AppExpr(AppExpr)
    | IF Expr THEN Expr ELSE Expr (If (Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match (Expr, MatchExpr))
    | NOT Expr (Prim1("!",Expr1))
    | NEGATE Expr (Prim1("-",Expr1))
    | HD Expr (Prim1("hd",Expr1))
    | TL Expr (Prim1("tl",Expr1))
    | ISE Expr (Prim1("ise",Expr1))
    | PRINT Expr (Prim1("print",Expr1))
    | Expr AND Expr (Prim2("andalso",Expr1,Expr2))
    | Expr PLUS Expr (Prim2("+",Expr1,Expr2))
    | Expr MINUS Expr (Prim2("-",Expr1,Expr2))
    | Expr MULTI Expr (Prim2("*",Expr1,Expr2))
    | Expr DIV Expr (Prim2("/",Expr1,Expr2))
    | Expr EQ Expr (Prim2("=",Expr1,Expr2))
    | Expr NOTEQ Expr (Prim2("!=",Expr1,Expr2))
    | Expr LESSER Expr (Prim2("<",Expr1,Expr2))
    | Expr LESSEREQ Expr (Prim2("<=",Expr1,Expr2))
    | Expr DOUBLEPOINT Expr (Prim2("::",Expr1,Expr2))
    | Expr SEMIC Expr (Prim2(";",Expr1,Expr2))
    | Expr LCOL CINT RCOL (Item(CINT,Expr))

AtomExpr : Const(Const)
    | NAME (Var(NAME))
    | LBRACKET Prog LBRACKET (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List Comps)
    | FUN Args FUNARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1,AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr,AtomExpr))

Const : CONBTRUE (ConB(TRUE)) | CONBFALSE (ConB(FALSE))
    | CINT (ConI(CINT))
    | LPAR RPAR (NIL)
    | LPAR TYPE LCOL RCOL RPAR (ESeq(Type))

Comps : Expr COMMA Expr (Expr1 :: Expr2 :: [])
    | Expr COMMA Comps (Expr :: Comps)

MatchExpr : END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr) :: MatchExpr)

CondExpr : Expr (SOME Expr)
    | UNDERSCORE (NONE)

Args : LPAR RPAR ([])
    | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar :: [])
    | TypedVar COMMA Params (TypedVar :: Params)

TypedVar : Type NAME ((Type, NAME))

Type : AtomType (AtomType)
    | LPAR Types RPAR (ListT Types)
    | LBRACKET Type RBRACKET (SeqT Type)
    | Type ARROW Type (FunT (Type1, Type2))

AtomType : NIL (ListT [])
    | BOOL (BoolT)
    | CINT (IntT)
    | LPAR Type RPAR (Type)

Types : Type COMMA Type (Type1 :: Type2 :: [])
    | Type COMMA Types (Type :: Types)