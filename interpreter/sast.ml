open Ast

type expressao = ExpInt of int pos
    | ExpVar of ident pos
    | ExpFloat of float pos
    | ExpChar of char pos
    | ExpString of string pos
    | ExpUn of (oper pos) * expressao
    | ExpBin of (oper pos) * expressao * expressao
    | ExpChamada of ident pos * (expressao expressoes)
