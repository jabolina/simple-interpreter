# load "lexical.cmo";;

let rec tokens lexbuf = 
  let tok = Lexical.token lexbuf in
  match tok with
    | Lexical.EOF -> [Lexical.EOF]
    | _ -> tok :: tokens lexbuf
;;

let analyze_str str =
  let lexbuf = Lexing.from_string str in
  tokens lexbuf
;;

let analyze_file path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  let toks = tokens lexbuf in
  let _ = close_in ic in toks
;;

