open Ast

type expressao = ExpInt of int * tipo
    | ExpVar of (expressao variavel) * tipo
    | ExpFloat of float * tipo
    | ExpChar of char * tipo
    | ExpString of string * tipo
    | ExpBool of bool * tipo
    | ExpVoid
    | ExpUn of (oper * tipo) * (expressao * tipo)
    | ExpBin of (oper * tipo) * (expressao * tipo) * (expressao * tipo)
    | ChamaFunc of ident * (expressao list) * tipo
