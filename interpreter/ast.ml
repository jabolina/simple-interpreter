open Lexing

type ident = string
type 'a pos =  'a * Lexing.position

type 'expr programa = Programa of declaracoes * ('expr funcao) list * ('expr comandos)
and 'expr funcoes = ('expr funcao) list
and declaracoes = declaracao list

and declaracao = CmdDec of (ident pos) * tipo
and 'expr funcao = Funcao of ('expr decfn)
and 'expr decfn = {
  fn_nome:    ident pos;
  fn_tiporet: tipo;
  fn_formais: (ident pos * tipo) list;
  fn_locais:  declaracoes;
  fn_corpo:   'expr comandos;
}

and 'expr comandos = ('expr comando) list
and 'expr comando =
  | CmdRetorno of 'expr option
  | CmdIf of 'expr * ('expr comandos) * ('expr comandos option)
  | CmdAtrib of 'expr * 'expr
  | CmdWhile of 'expr * 'expr comandos
  | CmdFor of 'expr comando * 'expr * 'expr comando * 'expr comandos
  | CmdEntrada of ('expr expressoes)
  | CmdSaida of ('expr expressoes)
  | CmdChamada of 'expr

and 'expr variaveis = ('expr variavel) list
and 'expr variavel =
  | VarSimples of ident pos
and 'expr expressoes = 'expr list

and tipo = TInt | TFloat | TChar | TString | TVoid

and oper = Mais | Menos | Mult | Div | Mod |
           Maior | Menor | MaiorI | MenorI |
           Igual | Difer | Ou | E | Nao
