open Analysis_Prelude
open NormalForm

(* expand_andSeparately take a formula and returns a list of formulas,
   where each element is a separately-conjunct of the original formula
   i.e. AndSeparately(e1, AndSeparately(e2, e3)) -> [e1; e2; e3]
 *)
let rec expand_andSeparately (formula : Formula.t) : Formula.t list = 
  match formula with
  | AndSeparately(exp1, exp2) -> 
    (expand_andSeparately exp1) @ (expand_andSeparately exp2)
  | _ -> [formula]

(* compress_andSeparately take a list of formulas and returns 
   an andSeparately between each item of the list
   i.e. [e1; e2; e3] -> AndSeparately(e1, AndSeparately(e2, e3))
 *)
and compress_andSeparately (formula : Formula.t list) : Formula.t =
  match formula with
  | []  -> EmptyHeap
  | [x] -> x
  | x::xs -> List.fold_left (fun x y -> Formula.AndSeparately(x, y)) x xs