module Amb = Ambiente
module A = Ast
module S = Sast
module T = Tast

let rec posicao exp = let open S in
  match exp with
  | ExpInt (_,pos) -> pos
  | ExpVar (_,pos) -> pos
  | ExpFloat (_,pos) -> pos
  | ExpChar (_,pos) -> pos
  | ExpString  (_,pos) -> pos
  | ExpUn ((_,pos),_) -> pos
  | ExpBin ((_,pos),_,_)  -> pos
  | ExpChamada ((_,pos), _) -> pos

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

let msg_erro_pos pos msg =
  let open Lexing in
  let lin = pos.pos_lnum
  and col = pos.pos_cnum - pos.pos_bol - 1 in
  Printf.sprintf "Semantico -> linha %d, coluna %d: %s" lin col msg

let msg_erro nome msg =
  let pos = snd nome in
  msg_erro_pos pos msg

let nome_tipo t =
  let open A in
    match t with
      TInt -> "inteiro"
    | TFloat -> "float"
    | TChar -> "char"
    | TString -> "string"
    | TVoid -> "void"

let mesmo_tipo pos msg tinf tdec =
  if tinf <> tdec
  then
    let msg = Printf.sprintf msg (nome_tipo tinf) (nome_tipo tdec) in
    failwith (msg_erro_pos pos msg)

let rec infere_exp amb exp =
  match exp with
    S.ExpInt n    -> (T.ExpInt (fst n, A.TInt),       A.TInt)
  | S.ExpString s -> (T.ExpString (fst s, A.TString), A.TString)
  | S.ExpFloat f -> (T.ExpFloat (fst f, A.TFloat), A.TFloat)
  | S.ExpChar c -> (T.ExpChar (fst c, A.TChar), A.TChar)
  | S.ExpVar v ->
    (match v with
       nome ->
       let id = fst nome in
         (try (match (Amb.busca amb id) with
               | Amb.EntVar tipo -> (T.ExpVar (A.VarSimples nome, tipo), tipo)
               | Amb.EntFun _ ->
                 let msg = "nome de funcao usado como nome de variavel: " ^ id in
                  failwith (msg_erro nome msg)
             )
          with Not_found ->
                 let msg = "A variavel " ^ id ^ " nao foi declarada" in
                 failwith (msg_erro nome msg)
         )
    )

  | S.ExpUn (op, valor) ->
    let (v, tv) = infere_exp amb valor in

    let verifica_unario () =
      (match tv with
        A.TInt -> tv
        | A.TString -> tv
        | A.TFloat -> tv
        | A.TChar -> tv

        | t -> let msg = "um operador relacional nao pode ser usado com o tipo " ^
            (nome_tipo t)
          in failwith (msg_erro_pos (snd op) msg)
      )

    in
    let op = fst op in
    let tinf = (match (classifica op) with
          Unario     -> verifica_unario ()
        | _ -> failwith "Encontrado operador binario para expressao unaria"
      )
    in
      (T.ExpUn ((op, tinf), (v, tv)), tv)

  | S.ExpBin (op, esq, dir) ->
    let (esq, tesq) = infere_exp amb esq
    and (dir, tdir) = infere_exp amb dir in

    let verifica_aritmetico () =
      (match tesq with
        A.TInt ->
          let _ = mesmo_tipo (snd op)
            "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
            tesq tdir
          in tesq
        | A.TFloat ->
          let _ = mesmo_tipo (snd op)
            "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
            tesq tdir
          in tesq

       | t -> let msg = "um operador aritmetico nao pode ser usado com o tipo " ^
                        (nome_tipo t)
         in failwith (msg_erro_pos (snd op) msg)
      )

    and verifica_relacional () =
      (match tesq with
        A.TInt ->
          let _ = mesmo_tipo (snd op)
            "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
            tesq tdir
          in tesq
        | A.TFloat ->
          let _ = mesmo_tipo (snd op)
            "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
            tesq tdir
          in tesq

        | t -> let msg = "um operador relacional nao pode ser usado com o tipo " ^
            (nome_tipo t)
          in failwith (msg_erro_pos (snd op) msg)
      )

    in
    let op = fst op in
    let tinf = (match (classifica op) with
          Aritmetico -> verifica_aritmetico ()
        | Relacional -> verifica_relacional ()
        | _ -> failwith "Unario encontrado em exp Binaria"
      )
    in
      (T.ExpBin ((op,tinf), (esq, tesq), (dir, tdir)), tesq)

  | S.ExpChamada (nome, args) ->
     let rec verifica_parametros ags ps fs =
        match (ags, ps, fs) with
         (a::ags), (p::ps), (f::fs) ->
            let _ = mesmo_tipo (posicao a)
                     "O parametro eh do tipo %s mas deveria ser do tipo %s" p f
            in verifica_parametros ags ps fs
       | [], [], [] -> ()
       | _ -> failwith (msg_erro nome "Numero incorreto de parametros")
     in
     let id = fst nome in
     try
       begin
         let open Amb in

         match (Amb.busca amb id) with
           Amb.EntFun {tipo_fn; formais} ->
           let argst = List.map (infere_exp amb) args
           and tipos_formais = List.map snd formais in
           let _ = verifica_parametros args (List.map snd argst) tipos_formais
            in (T.ChamaFunc (id, (List.map fst argst), tipo_fn), tipo_fn)
         | Amb.EntVar _ ->
           let msg = id ^ " eh uma variavel e nao uma funcao" in
           failwith (msg_erro nome msg)
       end
     with Not_found ->
       let msg = "Nao existe a funcao de nome " ^ id in
       failwith (msg_erro nome msg)

let rec verifica_cmd amb tiporet cmd =
  let open A in
  match cmd with
    CmdRetorno exp ->
    (match exp with
       None ->
        let _ = mesmo_tipo (Lexing.dummy_pos)
              "O tipo retornado eh %s mas foi declarado como %s"
              TVoid tiporet
        in CmdRetorno None
     | Some e ->
        let (e1,tinf) = infere_exp amb e in
        let _ = mesmo_tipo (posicao e)
                  "O tipo retornado eh %s mas foi declarado como %s"
                  tinf tiporet
        in CmdRetorno (Some e1)
      )
  | CmdIf (teste, entao, senao) ->
    let (teste1,tinf) = infere_exp amb teste
    and entao1 = List.map (verifica_cmd amb tiporet) entao in
    let senao1 =
      match senao with
        None -> None
        | Some bloco -> Some (List.map (verifica_cmd amb tiporet) bloco)
    in
      CmdIf (teste1, entao1, senao1)

  | CmdAtrib (elem, exp) ->
    let (exp,  tdir) = infere_exp amb exp
    and (elem1, tesq) = infere_exp amb elem in
    let _ = mesmo_tipo (posicao elem)
            "Atribuicao com tipos diferentes: %s = %s" tesq tdir
    in CmdAtrib (elem1, exp)

  | CmdWhile (teste, comandos) ->
    let (teste1, tinf) = infere_exp amb teste
    and corpo = List.map(verifica_cmd amb tiporet) comandos
    in
      CmdWhile (teste1, corpo)

  | CmdFor (atrib, teste, comando, corpo) ->
    let attr_externa = verifica_cmd amb tiporet atrib
    and (teste1, tinf) = infere_exp amb teste
    and attr_interna = verifica_cmd amb tiporet comando
    and corpo1 = List.map(verifica_cmd amb tiporet) corpo
    in
      CmdFor(attr_externa, teste1, attr_interna, corpo1)

  | CmdEntrada exps ->
    let exps = List.map (infere_exp amb) exps in
      CmdEntrada (List.map fst exps)

  | CmdSaida exps ->
    let exps = List.map (infere_exp amb) exps in
      CmdSaida (List.map fst exps)

  | CmdChamada exp ->
    let (exp, tinf) = infere_exp amb exp in
      CmdChamada exp

and verifica_fun amb ast =
  let open A in
  match ast with
    A.Funcao {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo} ->
      let ambfn = Amb.novo_escopo amb in
        let insere_parametro (v,t) = Amb.insere_param ambfn (fst v) t in
          let _ = List.iter insere_parametro fn_formais in
            let insere_local = fun (CmdDec (v,t)) -> Amb.insere_local ambfn (fst v)  t in
              let _ = List.iter insere_local fn_locais in
              let corpo_tipado = List.map (verifica_cmd ambfn fn_tiporet) fn_corpo in

              A.Funcao {
                fn_nome;
                fn_tiporet;
                fn_formais;
                fn_locais;
                fn_corpo = corpo_tipado
              }

let rec verifica_dup xs =
  match xs with
    [] -> []
  | (nome,t)::xs ->
    let id = fst nome in
    if (List.for_all (fun (n,t) -> (fst n) <> id) xs)
    then (id, t) :: verifica_dup xs
    else let msg = "Parametro duplicado " ^ id in
      failwith (msg_erro nome msg)

let insere_declaracao_var amb dec =
  let open A in
    match dec with
        CmdDec (nome, tipo) ->  Amb.insere_local amb (fst nome) tipo

let insere_declaracao_fun amb dec =
  let open A in
    match dec with
      Funcao {fn_nome; fn_tiporet; fn_formais; fn_corpo} ->
        let formais = verifica_dup fn_formais in
        let nome = fst fn_nome in
        Amb.insere_fun amb nome formais fn_tiporet

(* Lista de cabeçalhos das funções pré definidas *)
let fn_predefs = let open A in [
   ("printf", [("x", TInt); ("y", TInt)], TVoid);
   ("scanf",   [("x", TInt); ("y", TInt)], TVoid)
]

(* insere as funções pré definidas no ambiente global *)
let declara_predefinidas amb =
  List.iter (fun (n,ps,tr) -> Amb.insere_fun amb n ps tr) fn_predefs

let semantico ast =
  (* cria ambiente global inicialmente vazio *)
  let amb_global = Amb.novo_amb [] in
  let _ = declara_predefinidas amb_global in
  let (A.Programa (decs_globais, decs_funs, corpo)) = ast in
  let _ = List.iter (insere_declaracao_var amb_global) decs_globais in
  let _ = List.iter (insere_declaracao_fun amb_global) decs_funs in
  (* Verificação de tipos nas funções *)
  let decs_funs = List.map (verifica_fun amb_global) decs_funs in
  (* Verificação de tipos na função principal *)
  let corpo = List.map (verifica_cmd amb_global A.TVoid) corpo in
     (A.Programa (decs_globais, decs_funs, corpo),  amb_global)
