%{
    open Ast
    open Lexing
    open Sast
%}

%token <int * Lexing.position> LITINT
%token <string * Lexing.position> LITSTRING
%token <float * Lexing.position> LITFLOAT
%token <string * Lexing.position> ID
%token <Lexing.position> INT FLOAT CHAR VOID
%token <Lexing.position> ATRIB
%token <Lexing.position> APAR ACHA FPAR FCHA
%token <Lexing.position> MAIS MENOS MULT DIV MOD
%token <Lexing.position> ENTRADA SAIDA
%token <Lexing.position> PTVIRG VIRG
%token <Lexing.position> GTT LST
%token <Lexing.position> IGUAL DIFE LET GET
%token <Lexing.position> OU AND NAO
%token <Lexing.position> FOR IF ELSE WHILE
%token <Lexing.position> RET
%token EOF

%left OU
%left AND
%left IGUAL DIFE NAO
%left GTT LST
%left GET LET
%left MAIS MENOS
%left MULT DIV MOD

%start <Sast.expressao Ast.programa> program

%%

program: p = decl_fun* c = cmd_func*
    EOF { Programa (List.flatten [], p, c) }

decl_fun: tipo=types nome=ID APAR formais=separated_list(VIRG, argument) FPAR ACHA
        locais=cmd_dec*
        corpo=command*
    FCHA {
      Funcao {
        fn_nome = nome;
        fn_tiporet = tipo;
        fn_formais = formais;
        fn_locais = List.flatten locais;
        fn_corpo = corpo;
      }
    }

argument: types_non_void ID { ($2, $1) }

command:
    | cmd_attr PTVIRG   { $1 }
    | cmd_for           { $1 }
    | cmd_while         { $1 }
    | cmd_if            { $1 }
    | comando_entrada   { $1 }
    | comando_saida     { $1 }
    | cmd_func          { $1 }
    | cmd_ret           { $1 }

cmd_attr: ID ATRIB expression { CmdAtrib (ExpVar $1, $3) }

cmd_dec:
  t = types_non_void ids = separated_nonempty_list(VIRG, ID) PTVIRG {
    List.map (fun id -> CmdDec (id,t)) ids
  }

cmd_if: IF APAR expression FPAR ACHA
        command*
    FCHA cmd_else { CmdIf($3, $6, $8) }

cmd_else: { None }
    | ELSE ACHA c=command* FCHA { Some(c) }
    | ELSE cmd_if   { Some([$2]) }

cmd_while: WHILE APAR expression FPAR ACHA
            command*
        FCHA { CmdWhile($3,$6) }

cmd_for: FOR APAR cmd_attr PTVIRG expression PTVIRG cmd_attr FPAR ACHA
        command*
    FCHA { CmdFor($3, $5, $7, $10) }

cmd_func: call PTVIRG { CmdChamada($1) }

comando_entrada: ENTRADA xs=separated_nonempty_list(VIRG, expression) PTVIRG {
                   CmdEntrada xs
               }

comando_saida: SAIDA xs=separated_nonempty_list(VIRG, expression) PTVIRG {
                 CmdSaida xs
         }

call: ID APAR args=separated_list(VIRG, expression) FPAR {
  ExpChamada ($1, args)
}

cmd_ret: RET expression? PTVIRG { CmdRetorno $2 }

expression: LITINT          { ExpInt $1 }
    | LITFLOAT              { ExpFloat $1 }
    | LITSTRING             { ExpString $1 }
    | ID                    { ExpVar $1 }
    | APAR expression FPAR  { $2 }
    | call                  { $1 }
    | op=oper e=expression  { ExpUn (op, e) }
    | e=expression op=oper e1=expression { ExpBin(op, e, e1) }

%inline oper:
    | pos = MAIS   { (Mais, pos)  }
    | pos = MENOS  { (Menos, pos) }
    | pos = MULT   { (Mult, pos)  }
    | pos = DIV    { (Div, pos)   }
    | pos = MOD    { (Mod, pos)   }
    | pos = GTT    { (Maior, pos)   }
    | pos = LST    { (Menor, pos)   }
    | pos = GET    { (MaiorI, pos)   }
    | pos = LET    { (MenorI, pos)   }
    | pos = IGUAL  { (Igual, pos) }
    | pos = DIFE   { (Difer, pos)  }
    | pos = NAO    { (Nao, pos)  }
    | pos = OU     { (Ou, pos)    }
    | pos = AND    { (E, pos)   }

types:
    | INT { TInt }
    | FLOAT { TFloat }
    | CHAR { TChar }
    | LITSTRING { TString }
    | VOID { TVoid }

types_non_void:
    | INT { TInt }
    | FLOAT { TFloat }
    | CHAR { TChar }
    | LITSTRING { TString }
