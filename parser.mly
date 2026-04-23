
%{
  open Types;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LETREC
%token LET
%token IN
%token CONCAT
%token STRING
%token BOOL
%token NAT
%token TOP
%token BOT

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token COMMA
%token HASH
%token TYPE
%token LBRACE
%token RBRACE
%token SEMICOLON
%token LBRACKET
%token RBRACKET
%token HEAD
%token TAIL
%token ISEMPTY
%token COLON2
%token LABRACKET
%token RABRACKET
%token AS
%token CASE
%token OF
%token PIPE
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> TYIDV
%token <string> STRINGV

%start s
%type <Types.term> s

%%

s :
    term EOF
      { $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { 
        TmLetRec ($2, $4, $6, $8) 
      }
  | LET IDV EQ term
      {
        TmAssign($2,$4)
      }
  | LETREC IDV COLON ty EQ term
      {
        TmAssign($2, TmLetRec($2, $4, $6, AtTerm (TmVar $2)))
      }

appTerm :
    atomicTerm
      { $1 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | term HASH INTV
      { TmProj ($1, $3) }
  | HEAD term
      { TmHead $2 }
  | TAIL term
      { TmTail $2 }
  | TYPE TYIDV EQ LBRACE ty_fields RBRACE
      { TmAssign ($2, TmTyRecord ($2, $5)) }
  | TYIDV LBRACE record_fields RBRACE
      { TmRecord ($1, $3) }
  | TYPE TYIDV EQ LABRACKET ty_fields RABRACKET
      { TmAssign ($2, TmTyVariant ($2, $5)) }
  | TYPE TYIDV EQ ty
      { TmAssign ($2, TmTyAlias ($2, $4)) }
  | LABRACKET record_field RABRACKET AS TYIDV
      { TmVariant ($5, $2) }
  | term DOT IDV
      { TmProjLabel ($1, $3) }
  | CASE term OF variant_cases
      { TmCase ($2, $4) }

term_list :
    term COMMA term_list
      { $1 :: $3 }
  | term
      { [$1] }

ty_fields :
    
      { [] }
  | ty_fields_not_empty
      { $1 }

ty_fields_not_empty :
    ty_field SEMICOLON ty_fields
      { $1 :: $3 }
  | ty_field
      { [$1] }

ty_field :
    IDV COLON ty
      { ($1, $3) }

record_fields :
    
      { [] }
  | record_fields_not_empty
      { $1 }

record_fields_not_empty :
    record_field SEMICOLON record_fields
      { $1 :: $3 }
  | record_field
      { [$1] }

record_field :
    IDV EQ term
      { ($1, $3) }

variant_cases :
    variant_case PIPE variant_cases
      { $1 :: $3 }
  | variant_case
      { [$1] }

variant_case :
    LABRACKET IDV EQ IDV RABRACKET ARROW term
      { ($2, $4, $7) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LPAREN term_list RPAREN
      { TmTuple $2 }
  | LBRACKET term_list RBRACKET
      { TmList $2 }
  | LBRACKET RBRACKET
      { TmList [] }
  | LPAREN term COLON2 LBRACKET term_list RBRACKET RPAREN
      { TmCons($2, TmList $5) }
  | LPAREN term COLON2 LBRACKET RBRACKET RPAREN
      { TmCons($2, TmList []) }
  | LPAREN term COLON2 term RPAREN
      { TmCons($2, $4) }
  | LBRACE RBRACE
      { TmTyRecord ("{}",[]) }
  | TRUE
      { AtTerm (TmTrue) }
  | FALSE
      { AtTerm (TmFalse) }
  | SUCC term
      { AtTerm (TmSucc $2) }
  | PRED term
      { AtTerm (TmPred $2) }
  | ISZERO term
      { AtTerm (TmIsZero $2) }
  | ISEMPTY term
      { AtTerm (TmIsEmpty $2) }
  | STRINGV
      { AtTerm (TmString $1) }
  | IDV
      { AtTerm (TmVar $1) }
  | TYIDV
      { AtTerm (TmTyVar $1) }
  | INTV
      { let rec f = function
            0 -> AtTerm (TmZero)
          | n -> AtTerm (TmSucc (f (n-1)))
        in f $1 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LBRACE RBRACE
      { TyRecord ("empty_record",[]) }
  | TYIDV
      { AtTy (TyVar $1) }

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { AtTy (TyBool) }
  | NAT
      { AtTy (TyNat) }
  | STRING
      { AtTy (TyString) }
  | TOP
      { TyTop }
  | BOT
      { TyBot }