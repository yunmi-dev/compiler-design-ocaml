%{

module A = Absyn
module S = Symbol

let start_pos = Parsing.symbol_start
let end_pos = Parsing.symbol_end

%}

%start prog

%token COMMA SEMICOLON COLON
%token LPAREN RPAREN LT GT
%token PLUS MINUS TIMES
%token EQ
%token AND NOT OR
%token WHILE DO REF BANG ASSIGN
%token IF THEN ELSE
%token LET IN FUN ARROW TYPE UMINUS
%token EOF
%token <string> ID
%token <int> NUM PROJ

%type <Absyn.prog> prog
%type <Absyn.exp> exp
%type <Absyn.tp> tp

%right ARROW
%left SEMICOLON
%right ASSIGN
%left OR
%left AND
%nonassoc NOT
%left EQ LT
%left PLUS MINUS
%left TIMES
%right UMINUS
%left BANG REF
%left PROJ
%left COLON
%left LPAREN

%%

prog:
  | fundec_list EOF { $1 }

fundec_list:
  | fundec { [$1] }
  | fundec fundec_list { $1 :: $2 }

fundec:
  | FUN ID LPAREN ID COLON tp RPAREN COLON tp EQ exp
    { ((start_pos (), end_pos ()), (S.symbol $2, S.symbol $4, $6, $9, $11)) }

tp:
  | ID { 
      if $1 = "int" then A.Inttp 
      else A.Inttp (* treat unknown types as int for now *)
    }
  | LT tp_list GT { A.Tupletp($2) }
  | tp ARROW tp { A.Arrowtp($1, $3) }
  | tp REF { A.Reftp($1) }
  | LPAREN tp RPAREN { $2 }

tp_list:
  | /* empty */ { [] }
  | tp { [$1] }
  | tp COMMA tp_list { $1 :: $3 }

exp:
  | atomic_exp { $1 }
  | unary_exp { $1 }
  | binary_exp { $1 }
  | control_exp { $1 }

atomic_exp:
  | NUM { A.Pos((start_pos (), end_pos ()), A.Int($1)) }
  | ID { A.Pos((start_pos (), end_pos ()), A.Id(S.symbol $1)) }
  | LPAREN exp RPAREN { $2 }
  | LT exp_list GT { A.Pos((start_pos (), end_pos ()), A.Tuple($2)) }

unary_exp:
  | MINUS exp %prec UMINUS { A.Pos((start_pos (), end_pos ()), A.Op(A.Sub, [A.Int(0); $2])) }
  | NOT exp { 
      A.Pos((start_pos (), end_pos ()), 
            A.If($2, A.Int(0), A.Int(1)))
    }
  | BANG exp { A.Pos((start_pos (), end_pos ()), A.Op(A.Get, [$2])) }
  | REF exp { A.Pos((start_pos (), end_pos ()), A.Op(A.Ref, [$2])) }
  | PROJ exp { A.Pos((start_pos (), end_pos ()), A.Proj($1, $2)) }

binary_exp:
  | exp PLUS exp { A.Pos((start_pos (), end_pos ()), A.Op(A.Add, [$1; $3])) }
  | exp MINUS exp { A.Pos((start_pos (), end_pos ()), A.Op(A.Sub, [$1; $3])) }
  | exp TIMES exp { A.Pos((start_pos (), end_pos ()), A.Op(A.Mul, [$1; $3])) }
  | exp LT exp { A.Pos((start_pos (), end_pos ()), A.Op(A.LT, [$1; $3])) }
  | exp EQ exp { A.Pos((start_pos (), end_pos ()), A.Op(A.Eq, [$1; $3])) }
  | exp AND exp { 
      A.Pos((start_pos (), end_pos ()), 
            A.If($1, A.If($3, A.Int(1), A.Int(0)), A.Int(0)))
    }
  | exp OR exp { 
      A.Pos((start_pos (), end_pos ()), 
            A.If($1, A.Int(1), A.If($3, A.Int(1), A.Int(0))))
    }
  | exp ASSIGN exp { A.Pos((start_pos (), end_pos ()), A.Op(A.Set, [$1; $3])) }
  | exp COLON tp { A.Pos((start_pos (), end_pos ()), A.Constrain($1, $3)) }
  | exp SEMICOLON exp { A.Pos((start_pos (), end_pos ()), A.Let(S.symbol "_", $1, $3)) }
  | exp LPAREN exp RPAREN { A.Pos((start_pos (), end_pos ()), A.Call($1, $3)) }

control_exp:
  | IF exp THEN exp ELSE exp { A.Pos((start_pos (), end_pos ()), A.If($2, $4, $6)) }
  | IF exp THEN exp { A.Pos((start_pos (), end_pos ()), A.If($2, $4, A.Tuple([]))) }
  | WHILE exp DO exp { A.Pos((start_pos (), end_pos ()), A.While($2, $4)) }
  | LET ID EQ exp IN exp { A.Pos((start_pos (), end_pos ()), A.Let(S.symbol $2, $4, $6)) }

exp_list:
  | /* empty */ { [] }
  | exp { [$1] }
  | exp COMMA exp_list { $1 :: $3 }

%%