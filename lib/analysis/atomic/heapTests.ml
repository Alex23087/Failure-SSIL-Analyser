open AtomicBase
open Normalization
open DataStructures.Analysis.NormalForm

open Analysis_TestCommon

(* Alias for better readability *)
module Commands = Ast.HeapRegularCommands
module PFormula = DataStructures.Parser.LogicFormulas.Formula
module PBinaryComparison = DataStructures.Parser.LogicFormulas.BinaryComparison
module PArithmeticExpression = DataStructures.Parser.LogicFormulas.ArithmeticExpression
module PBinaryOperator = DataStructures.Parser.LogicFormulas.BinaryOperator

(************************** Alloc tests ****************************)

(* << emp >> x := alloc() << x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << x -> v >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
  let post_condition =
    annot (PFormula.Allocation(
      "x",
      annot (PArithmeticExpression.Variable("v"))
    )
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.EmptyHeap :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints

(* << false >> x := alloc() << x -/> v >> *)
let%test "precondition on x := alloc(), post-condition = << x -/> >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = annot ( PFormula.NonAllocated("x") ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << false >> x := alloc() << y -> v >> *)
let%test "precondition on x := alloc(), post-condition = << y -> v >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
  let post_condition =
    annot (PFormula.Allocation(
      "y",
      annot (PArithmeticExpression.Variable("v"))
    )
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.False :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints

(* << false >> x := alloc() << Exists x . x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << Exists x . x -> v >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
  let post_condition =
    annot (PFormula.Exists("x", 
      annot (PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("v"))
        )
      )
    )
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.False :: [] in
  test_expected_bound_variables pre_condition 1 &&
  test_expected_disjoints pre_condition expected_disjoints

(* << false >> x := alloc() << false >> *)
let%test "precondition on x := alloc(), post-condition = << false >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
  let post_condition = annot (PFormula.False) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.False :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints

(* << true >> x := alloc() << true >> *)
let%test "precondition on x := alloc(), post-condition = << true >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
  let post_condition = annot (PFormula.True) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.True :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints

(* << false >> x := alloc() << emp >> *)
let%test "precondition on x := alloc(), post-condition = << emp >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = annot ( PFormula.EmptyHeap ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << false || false >> x := alloc() << emp || emp >> *)
let%test "precondition on x := alloc(), post-condition = << emp || emp >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.Or(
      annot (PFormula.EmptyHeap),
      annot (PFormula.EmptyHeap)
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << false || emp >> x := alloc() << emp || x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << emp || x -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.Or(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.EmptyHeap :: Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << false >> x := alloc() << emp && x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << emp && x -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.And(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(************************** Frame Rule on Alloc tests ****************************)

(* << emp * emp >> x := alloc() << emp * x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << emp * x -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.AndSeparately( Formula.EmptyHeap, Formula.EmptyHeap ) :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << false >> x := alloc() << emp * y -> v >> *)
let%test "precondition on x := alloc(), post-condition = << emp * y -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << false >> x := alloc() << emp * x -/> >> *)
let%test "precondition on x := alloc(), post-condition = << emp * y -/> >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.NonAllocated("x")
      )
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << y -> v * emp >> x := alloc() << y -> v * x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << y -> v * x -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (
        PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("v"))
      )),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(Formula.EmptyHeap, Formula.Allocation("y", Variable("v"))) :: [] 
in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << x -/> * emp >> x := alloc() << x -/> * x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << x -/> * x -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.NonAllocated("x")),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << emp * true >> x := alloc() << emp * true >> *)
let%test "precondition on x := alloc(), post-condition = << emp * true >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (PFormula.True)
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(Formula.EmptyHeap, Formula.True) :: [] 
in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << true * emp >> x := alloc() << true * x -> v >> *)
let%test "precondition on x := alloc(), post-condition = << true * x -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.True),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(Formula.EmptyHeap, Formula.True) :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << true * false >> x := alloc() << true * y -> v >> *)
let%test "precondition on x := alloc(), post-condition = << true * y -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.True),
      annot (
        PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("v"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(Formula.True, Formula.Allocation("y", Variable("v"))) :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 

(* << v = 5 * emp * y -> v >> x := alloc() << v = 5 * x -> v * y -> v >> *)
let%test "precondition on x := alloc(), post-condition = << v = 5 * x -> v * y -> v >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.Allocation("x")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.Allocation("v", (annot (PArithmeticExpression.Literal(5) ) ) ) ),
      annot (PFormula.AndSeparately(
        annot (PFormula.Allocation("x", annot (PArithmeticExpression.Variable("v") ) ) ),
        annot (PFormula.Allocation("y", annot (PArithmeticExpression.Variable("v") ) ) ) 
      )
    ))
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(
    Formula.EmptyHeap, 
    Formula.AndSeparately(
      Formula.Allocation("v", Literal(5)), 
      Formula.Allocation("y", Variable("v"))
    )
  ) :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints 