{
  open Lexing
  open Printf

  let inc_nb_lines lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  let msg_err lexbuf c =
    let pos = lexbuf.lex_curr_p in
    let lin = pos.pos_lnum
    and col = pos.pos_cnum - pos.pos_bol - 1 in
    sprintf "%d-%d: unknown character %c" lin col c

  let err link col msg =
    let message = sprintf "%d-%d: %s" link col msg in
      failwith message


    type tokens = APAR
            | ACHA
            | FCHA
            | ACOL
            | FCOL
            | FPAR
            | ATRIB
            | SENAO
            | ENTAO
            | IF
            | ELSE
            | WHILE
            | SWITCH
            | CASE
            | BREAK
            | PTVIRG
            | VIRG
            | MAIS
            | MENOS
            | MULT
            | DIV
            | MOD
            | ECOM
            | OU
            | NAO
            | IGUAL
            | DIFE
            | LST
            | GTT
            | LET
            | GET
            | DO
            | FOR
            | RET
            | INT
            | FLOAT
            | DOUBLE
            | CHAR
            | VOID
            | LITINT of int
            | LITSTRING of string
            | LITFLOAT of float
            | ID of string
            | EOF
}

let digit = ['0' - '9']
let integer = digit+
let double = (digit+ "." digit*) | (digit+ "." digit+)
let leter = ['a' - 'z' 'A' - 'Z']
let lib = "(<|\")?\\s*" leter+ "\\s*(>|\")?"
let preproc = "#" [^ '\r' '\n']*
let identifier = leter(leter|digit|'_')*
let blank = [' ' '\t']+
let newline = ['\r' '\n']+
let comment = "//" [^ '\r' '\n']*


rule token = parse
    blank   {token lexbuf}
  | newline {inc_nb_lines lexbuf; token lexbuf}
  | comment {token lexbuf}
  | preproc {token lexbuf}
  | "/*"    {nested_comments 0 lexbuf}
  | ","     {VIRG}
  | ":"     {SENAO}
  | "?"     {ENTAO}
  | ";"     {PTVIRG}
  | '['     {ACOL}
  | '('     {APAR}
  | '{'     {ACHA}
  | '+'     {MAIS}
  | '-'     {MENOS}
  | "*"     {MULT}
  | '/'     {DIV}
  | '%'     {MOD}
  | '&'     {ECOM}
  | '|'     {OU}
  | "!="    {DIFE}
  | '!'     {NAO}
  | "<="    {LET}
  | ">="    {GET}
  | "<"     {LST}
  | ">"     {GTT}
  | ')'     {FPAR}
  | '}'     {FCHA}
  | ']'     {FCOL}
  | "=="    {IGUAL}
  | '='     {ATRIB}
  | double as num {let numero = float_of_string num in LITFLOAT numero}
  | integer as num {let numero = int_of_string num in LITINT numero}
  | "void"  {VOID}
  | "int"   {INT}
  | "float" {FLOAT}
  | "double" {DOUBLE}
  | "char"  {CHAR}
  | "do"    {DO}
  | "for"   {FOR}
  | "if"    {IF}
  | "else"  {ELSE}
  | "while" {WHILE}
  | "switch" {SWITCH}
  | "case"  {CASE}
  | "break" {BREAK}
  | "return" {RET}
  | identifier as id {ID id}
  | '"'     {let pos = lexbuf.lex_curr_p in
             let lin = pos.pos_lnum
             and col = pos.pos_cnum - pos.pos_bol - 1 in
             let buffer = Buffer.create 1 in
             let str = read_string lin col buffer lexbuf in LITSTRING str}
  | _ as c  {failwith (msg_err lexbuf c)}
  | eof     {EOF}

and nested_comments n = parse
    "*/"    {if n=0 then token lexbuf
             else nested_comments (n-1) lexbuf}
  | "/*"    {nested_comments (n+1) lexbuf}
  | newline {inc_nb_lines lexbuf; nested_comments n lexbuf}
  | _       {nested_comments n lexbuf}
  | eof     {failwith "Comment block not closed."}

and read_string lin col buffer = parse
    '"'     {Buffer.contents buffer}
  | "\\t"   {Buffer.add_char buffer '\t'; read_string lin col buffer lexbuf}
  | "\\n"   {Buffer.add_char buffer '\n'; read_string lin col buffer lexbuf}
  | '\\' '"'{Buffer.add_char buffer '"'; read_string lin col buffer lexbuf}
  | '\\' '\\' {Buffer.add_char buffer '\\'; read_string lin col buffer lexbuf}
  | _ as c  {Buffer.add_char buffer c; read_string lin col buffer lexbuf}
  | eof     {err lin col "String not closed."}

