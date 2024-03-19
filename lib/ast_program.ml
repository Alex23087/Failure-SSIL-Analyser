(* Grammar:
   RCmd ::=  ACmd  |  RCmd; RCmd  |  RCmd + RCmd  |  RCmd*
   
   ACmd ::=  SKIP  |  Ident = AExp  |  BExp ?
   
   BExp ::=  TRUE  |  FALSE  |  !Bexp  |  BExp && BExp  |  BExp || BExp  |  AExp Comp AExp
   
   Comp ::=  =  |  !=  |  <  |  <=  |  >  |  >=
   
   AExp ::=  INT(n)  |  Ident  |  AExp Binop AExp
   
   Binop ::=  +  |  -  |  *  |  /  
*)

module type AnnotationType = sig
  type t
end

module ASTRegularCommands(Annot: AnnotationType) = struct
  type t = Annot.t
  type identifier = string [@@deriving show]
  type 'a annotated_node = {node: 'a; annotation: t [@opaque]} [@@deriving show]
  let (@@) node = match node with {node = _; annotation} -> annotation
  let (@!) node = match node with {node; annotation = _} -> node

  type aop =
    | Plus
    | Minus
    | Times
    | Div
  [@@deriving show]

  type comp =
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge
  [@@deriving show]

  type aexp_node =
    | Literal of int
    | Var of identifier
    | Binop of aop * aexp * aexp
  and aexp = aexp_node annotated_node
  [@@deriving show]

  type bexp_node =
    | True
    | False
    | Not of bexp
    | And of bexp * bexp
    | Or of bexp * bexp
    | Comp of comp * aexp * aexp
  and bexp = bexp_node annotated_node
  [@@deriving show]

  type acmd_node =
    | Skip
    | Assign of identifier * aexp
    | Guard of bexp
  and acmd = acmd_node annotated_node
  [@@deriving show]

  type rcmd_node =
    | Command of acmd
    | Seq of rcmd * rcmd
    | Nondet of rcmd * rcmd
    | Star of rcmd
  and rcmd = rcmd_node annotated_node
  [@@deriving show]
end