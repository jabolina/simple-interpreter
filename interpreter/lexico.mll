{
  open Lexing
  open Printf
  open Sintatico

  exception Erro of string

  let incr_num_linha lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1;
                 pos_bol = pos.pos_cnum
      }

  let pos_atual lexbuf = lexbuf.lex_start_p
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
  | newline {incr_num_linha lexbuf; token lexbuf}
  | comment {token lexbuf}
  | preproc {token lexbuf}
  | "/*"    {nested_comments 0 lexbuf}
  | ","     {VIRG (pos_atual lexbuf)}
  | ";"     {PTVIRG (pos_atual lexbuf)}
  | "("     {APAR (pos_atual lexbuf)}
  | "{"     {ACHA (pos_atual lexbuf)}
  | "+"     {MAIS (pos_atual lexbuf)}
  | "-"     {MENOS (pos_atual lexbuf)}
  | "*"     {MULT (pos_atual lexbuf)}
  | "/"     {DIV (pos_atual lexbuf)}
  | "%"     {MOD (pos_atual lexbuf)}
  | "&&"    {AND (pos_atual lexbuf)}
  | "||"    {OU (pos_atual lexbuf)}
  | "!="    {DIFE (pos_atual lexbuf)}
  | "!"     {NAO (pos_atual lexbuf)}
  | "<="    {LET (pos_atual lexbuf)}
  | ">="    {GET (pos_atual lexbuf)}
  | "<"     {LST (pos_atual lexbuf)}
  | ">"     {GTT (pos_atual lexbuf)}
  | ")"     {FPAR (pos_atual lexbuf)}
  | "}"     {FCHA (pos_atual lexbuf)}
  | "=="    {IGUAL (pos_atual lexbuf)}
  | "="     {ATRIB (pos_atual lexbuf)}
  | double as num { LITFLOAT (float_of_string num, pos_atual lexbuf) }
  | integer as num { LITINT (int_of_string num, pos_atual lexbuf) }
  | "void"  {VOID (pos_atual lexbuf)}
  | "int"   {INT (pos_atual lexbuf)}
  | "float" {FLOAT (pos_atual lexbuf)}
  | "char"  {CHAR (pos_atual lexbuf)}
  | "for"   {FOR (pos_atual lexbuf)}
  | "if"    {IF (pos_atual lexbuf)}
  | "else"  {ELSE (pos_atual lexbuf)}
  | "while" {WHILE (pos_atual lexbuf)}
  | "return" {RET (pos_atual lexbuf)}
  | "printf" { SAIDA (pos_atual lexbuf)}
  | "scanf" { ENTRADA (pos_atual lexbuf)}
  | identifier as id {ID (id, pos_atual lexbuf)}
  | "\""     { let buffer = Buffer.create 1 in
              let str = read_string buffer lexbuf in
              LITSTRING (str, pos_atual lexbuf) }
  | _       {raise (Erro ("Caracter desconhecido: " ^ Lexing.lexeme lexbuf))}
  | eof     {EOF}

and nested_comments n = parse
  "*/"   { if n=0 then token lexbuf
           else nested_comments (n-1) lexbuf }
  | "/*"    { nested_comments (n+1) lexbuf }
  | _       { nested_comments n lexbuf }
  | eof     { raise (Erro "Comentário não terminado") }

and read_string buffer = parse
  '"'      { Buffer.contents buffer}
  | "\\t"     { Buffer.add_char buffer '\t'; read_string buffer lexbuf }
  | "\\n"     { Buffer.add_char buffer '\n'; read_string buffer lexbuf }
  | '\\' '"'  { Buffer.add_char buffer '"'; read_string buffer lexbuf }
  | '\\' '\\' { Buffer.add_char buffer '\\'; read_string buffer lexbuf }
  | _ as c    { Buffer.add_char buffer c; read_string buffer lexbuf }
  | eof       { raise (Erro "A string não foi terminada") }
