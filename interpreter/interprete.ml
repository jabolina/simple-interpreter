open Char

module Amb = AmbInterp
module A = Ast
module S = Sast
module T = Tast

exception Valor_de_retorno of T.expressao

let obtem_nome_tipo_var exp = let open T in
  match exp with
  | ExpVar (v,tipo) ->
    (match v with
      | A.VarSimples (nome,_) -> (nome,tipo)
    )
  | _ -> failwith "obtem_nome_tipo_var: nao eh variavel"

let pega_int_ou_bool exp =
  match exp with
  |  T.ExpInt (i,_) -> i
  | T.ExpBool (b,_) ->
    (match b with
      true -> 1
    | false -> 0
    )
  | _ -> failwith "pega_int: nao eh inteiro"

let pega_float_ou_bool exp =
  match exp with
  | T.ExpFloat (f,_)-> f
  | T.ExpBool (b,_) ->
    (match b with
      true -> 1.0
    | false -> 0.0
    )
  | _ -> failwith "pega_float: nao eh float"

let pega_char_ou_bool exp =
  match exp with
  | T.ExpChar (c,_) -> escaped c
  | T.ExpBool (b,_) ->
    (match b with
      true -> "a"
    | false -> ""
    )
  | _ -> failwith "pega_char: nao eh char"

let pega_string_ou_bool exp =
  match exp with
  |  T.ExpString (s,_) -> s
  | T.ExpBool (b,_) ->
    (match b with
      true -> "a"
    | false -> ""
    )
  | _ -> failwith "pega_string: nao eh string"


let pega_int exp =
  match exp with
  |  T.ExpInt (i,_) -> i
  | _ -> failwith "pega_int: nao eh inteiro"

let pega_float exp =
  match exp with
  | T.ExpFloat (f,_)-> f
  | _ -> failwith "pega_float: nao eh float"

let pega_char exp =
  match exp with
  | T.ExpChar (c,_) -> escaped c
  | _ -> failwith "pega_char: nao eh char"

let pega_string exp =
  match exp with
  |  T.ExpString (s,_) -> s
  | _ -> failwith "pega_string: nao eh string"

type classe_op = Aritmetico | Relacional | Unario

let classifica op =
  let open A in
  match op with
  | Nao -> Unario
  | Ou
  | E
  | Menor
  | Maior
  | MaiorI
  | MenorI
  | Igual
  | Difer -> Relacional
  | Mais
  | Menos
  | Mult
  | Div
  | Mod -> Aritmetico


let rec interpreta_exp amb exp =
  let open A in
  let open T in
  match exp with
  | ExpVoid
  | ExpInt _
  | ExpFloat _
  | ExpString _
  | ExpChar _
  | ExpBool _ -> exp
  | ExpVar _ ->
    let (id,tipo) = obtem_nome_tipo_var exp in
    (match (Amb.busca amb id) with
     | Amb.EntVar (tipo, v) ->
       (match v with
        | None -> failwith ("variável nao inicializada: " ^ id)
        | Some valor -> valor
       )
     |  _ -> failwith "interpreta_exp: expvar"
    )

  | ExpBin ((op,top), (esq, tesq), (dir,tdir)) ->
    let vesq = interpreta_exp amb esq
    and vdir = interpreta_exp amb dir in

      let interpreta_aritmetico () =
        (match tesq with
          | TInt ->
            (match op with
              | Mais ->     ExpInt (pega_int vesq + pega_int vdir, top)
              | Menos -> ExpInt (pega_int vesq - pega_int vdir, top)
              | Mult ->     ExpInt (pega_int vesq * pega_int vdir, top)
              | Div  ->      ExpInt (pega_int vesq / pega_int vdir, top)
              | Mod  ->      ExpInt (pega_int vesq mod pega_int vdir, top)
              | _ -> failwith "interpreta_aritmetico"
            )

          | TFloat ->
            (match op with
              | Mais -> ExpFloat (pega_float vesq +. pega_float vdir, top)
              | Menos -> ExpFloat (pega_float vesq -. pega_float vdir, top)
              | Mult -> ExpFloat (pega_float vesq *. pega_float vdir, top)
              | Div -> ExpFloat (pega_float vesq /. pega_float vdir, top)
              | Mod -> ExpFloat (mod_float (pega_float vesq) (pega_float vdir), top)
              | _ -> failwith "interpreta_aritmetico"
            )
          | _ -> failwith "Valor nao permitido para operador aritmetico"
        )

        and interpreta_relacional () =
          (match tesq with
            | TInt ->
              (match op with
                | Menor -> ExpBool (pega_int vesq < pega_int vdir, top)
                | Maior -> ExpBool (pega_int vesq > pega_int vdir, top)
                | Igual -> ExpBool (pega_int vesq == pega_int vdir, top)
                | Difer -> ExpBool (pega_int vesq != pega_int vdir, top)
                | MenorI -> ExpBool (pega_int vesq <= pega_int vdir, top)
                | MaiorI -> ExpBool (pega_int vesq >= pega_int vdir, top)
                | Ou -> ExpBool ((pega_int_ou_bool vesq) != 0  || (pega_int_ou_bool vdir) != 0, top)
                | E -> ExpBool ((pega_int_ou_bool vesq) != 0  && (pega_int_ou_bool vdir) != 0, top)
                | _ -> failwith "interpreta_relacional"
              )

            | TFloat ->
              (match op with
                | Menor -> ExpBool (pega_float vesq < pega_float vdir, top)
                | Maior  -> ExpBool (pega_float vesq > pega_float vdir, top)
                | Igual   -> ExpBool (pega_float vesq == pega_float vdir, top)
                | Difer   -> ExpBool (pega_float vesq != pega_float vdir, top)
                | MenorI -> ExpBool (pega_float vesq <= pega_float vdir, top)
                | MaiorI -> ExpBool (pega_float vesq >= pega_float vdir, top)
                | Ou -> ExpBool ((pega_float_ou_bool vesq) != 0.0  || (pega_float_ou_bool vdir) != 0.0, top)
                | E -> ExpBool ((pega_float_ou_bool vesq) != 0.0  && (pega_float_ou_bool vdir) != 0.0, top)
                | _ -> failwith "interpreta_relacional"
              )

            | TString ->
              (match op with
                | Menor -> ExpBool (pega_string vesq < pega_string vdir, top)
                | Maior  -> ExpBool (pega_string vesq > pega_string vdir, top)
                | Igual   -> ExpBool (pega_string vesq == pega_string vdir, top)
                | Difer   -> ExpBool (pega_string vesq != pega_string vdir, top)
                | MenorI -> ExpBool (pega_string vesq <= pega_string vdir, top)
                | MaiorI -> ExpBool (pega_string vesq >= pega_string vdir, top)
                | Ou -> ExpBool ((pega_string_ou_bool vesq) != ""  || (pega_string_ou_bool vdir) != "", top)
                | E -> ExpBool ((pega_string_ou_bool vesq) != ""  && (pega_string_ou_bool vdir) != "", top)
                | _ -> failwith "interpreta_relacional"
              )

            | TChar ->
              (match op with
                | Menor -> ExpBool (pega_char vesq < pega_char vdir, top)
                | Maior  -> ExpBool (pega_char vesq > pega_char vdir, top)
                | Igual   -> ExpBool (pega_char vesq == pega_char vdir, top)
                | Difer   -> ExpBool (pega_char vesq != pega_char vdir, top)
                | MenorI -> ExpBool (pega_char vesq <= pega_char vdir, top)
                | MaiorI -> ExpBool (pega_char vesq >= pega_char vdir, top)
                | Ou -> ExpBool ((pega_char_ou_bool vesq) != ""  || (pega_char_ou_bool vdir) != "", top)
                | E -> ExpBool ((pega_char_ou_bool vesq) != ""  && (pega_char_ou_bool vdir) != "", top)
                | _ -> failwith "interpreta_relacional"
              )

            | _ ->  failwith "interpreta_relacional"
          )

    in
    let valor = (match (classifica op) with
          Aritmetico -> interpreta_aritmetico ()
        | Relacional -> interpreta_relacional ()
        | _ -> failwith "Operado diferente de binario em exp binaria"
      )
    in
      valor

  | ExpUn ((op,top), (dir,tdir)) ->
    let valor = interpreta_exp amb dir in
      let interpreta_unario () =
        (match tdir with
          | TInt ->
            (match op with
              Nao -> ExpBool (not (pega_int_ou_bool valor != 0), top)
            | _ -> failwith "interpreta_unario")
          | TFloat ->
            (match op with
              Nao -> ExpBool (not (pega_float_ou_bool valor != 0.0), top)
            | _ -> failwith "interpreta_unario")
          | TString ->
            (match op with
              Nao -> ExpBool (not (pega_string_ou_bool valor != ""), top)
            | _ -> failwith "interpreta_unario")
          | TChar ->
            (match op with
              Nao -> ExpBool (not (pega_char_ou_bool valor != ""), top)
            | _ -> failwith "interpreta_unario")
          | _ -> failwith "interpreta_unario"
        )
    in
      let valor = (match (classifica op) with
            Unario -> interpreta_unario ()
          | _ -> failwith "Operado diferente de unario em exp unaria"
        )
      in
        valor

  | ChamaFunc (id, args, tipo) ->
    let open Amb in
    ( match (Amb.busca amb id) with
      | Amb.EntFun {tipo_fn; formais; locais; corpo} ->
           let vargs = List.map (interpreta_exp amb) args in
           let vformais = List.map2 (fun (n,t) v -> (n, t, Some v)) formais vargs
           in interpreta_fun amb id vformais locais corpo
      | _ -> failwith "interpreta_exp: expchamada"
    )

and interpreta_fun amb fn_nome fn_formais fn_locais fn_corpo =
  let open A in
  let ambfn = Amb.novo_escopo amb in
   let insere_local  d =
    match d with
      (CmdDec (v,t)) -> Amb.insere_local ambfn (fst v)  t None
  in
  let insere_parametro (n,t,v) = Amb.insere_param ambfn n t v in
  let _ = List.iter insere_parametro fn_formais in
    let _ = List.iter insere_local fn_locais in
  try
    let _ = List.iter (interpreta_cmd ambfn) fn_corpo in T.ExpVoid
    with
       Valor_de_retorno expret -> expret

and interpreta_cmd amb cmd =
  let open A in
  let open T in
  match cmd with
  CmdRetorno exp ->
    (match exp with
       None -> raise (Valor_de_retorno ExpVoid)
     | Some e ->
       let e1 = interpreta_exp amb e in
       raise (Valor_de_retorno e1)
    )

  | CmdIf (teste, entao, senao) ->
    let teste1 = interpreta_exp amb teste in
    (match teste1 with
       ExpBool (true,_) ->
       List.iter (interpreta_cmd amb) entao
     | _ ->
       (match senao with
          None -> ()
        | Some bloco -> List.iter (interpreta_cmd amb) bloco
       )
    )

  | CmdAtrib (elem, exp) ->
      let exp = interpreta_exp amb exp
      and (elem1,tipo) = obtem_nome_tipo_var elem in
        Amb.atualiza_var amb elem1 tipo (Some exp)

  | CmdWhile (teste, corpo) ->
    let teste1 = interpreta_exp amb teste in
    (match teste1 with
      ExpBool (true, _) ->
        let entao1 = corpo @ [CmdWhile(teste, corpo)] in
        List.iter (interpreta_cmd amb) entao1
      | _ -> ()
    )

  | CmdFor (attr_esq, teste, attr_dir, corpo) ->
    let _ = interpreta_cmd amb attr_esq
    and teste1 = interpreta_exp amb teste in
      (match teste1 with
        ExpBool (true, _) ->
          let corpo1 = corpo @ [CmdFor(attr_dir, teste, attr_dir, corpo)] in
          List.iter (interpreta_cmd amb) corpo1
        | _ -> ()
      )

  | CmdEntrada exps ->
    let nts = List.map (obtem_nome_tipo_var) exps in
    let leia_var (nome,tipo) =
      let valor =
        (match tipo with
          | A.TInt    -> T.ExpInt    (read_int (),  tipo)
          | A.TFloat  -> T.ExpFloat  (read_float (), tipo)
          | A.TString -> T.ExpString (read_line (), tipo)
          | _ -> failwith "leia_var: nao implementado"
        )
      in Amb.atualiza_var amb nome tipo (Some valor)
    in
      List.iter leia_var nts

  | CmdSaida exps ->
    let exps = List.map (interpreta_exp amb) exps in
    let imprima exp =
      (match exp with
        | T.ExpInt (n,_) -> print_int n
        | T.ExpFloat (f,_) -> let _ = print_float f in print_string " "
        | T.ExpString (s,_) -> let _ = print_string s in print_string " "
        | T.ExpBool (b,_) ->
          let _ = print_string (if b then "true" else "false")
            in print_string " "
        | _ -> failwith "imprima: nao implementado"
      )
    in
      let _ = List.iter imprima exps in
      print_newline ()

  | CmdChamada exp -> ignore( interpreta_exp amb exp)

let insere_declaracao_var amb dec =
    match dec with
        A.CmdDec (nome, tipo) ->  Amb.insere_local amb (fst nome) tipo None

let insere_declaracao_fun amb dec =
  let open A in
    match dec with
      Funcao {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo} ->
        let nome = fst fn_nome in
        let formais = List.map (fun (n,t) -> ((fst n), t)) fn_formais in
        Amb.insere_fun amb nome formais fn_locais fn_tiporet fn_corpo

let fn_predefs = let open A in [
    ("printf", [("x", TInt); ("y", TInt)], TVoid, []);
    ("scanf",  [("x", TInt); ("y", TInt)], TVoid, []);
]

let declara_predefinidas amb =
  List.iter (fun (n,ps,tr,c) -> Amb.insere_fun amb n ps [] tr c) fn_predefs

let interprete ast =
  let amb_global = Amb.novo_amb [] in
  let _ = declara_predefinidas amb_global in
  let (A.Programa (decs_globais, decs_funs, corpo)) = ast in
  let _ = List.iter (insere_declaracao_var amb_global) decs_globais in
  let _ = List.iter (insere_declaracao_fun amb_global) decs_funs in
  let resultado = List.iter (interpreta_cmd amb_global) corpo in
  resultado
