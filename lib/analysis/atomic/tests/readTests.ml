open AtomicBase
open Normalization
open DataStructures.Analysis.NormalForm
open Analysis_TestUtils
open DataStructures

(* << y -> a >> x := [y] << y -> a >> *)
let%test "precondition on x := [y], post-condition = << x -> y >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition =
    annot (PFormula.Allocation(
      "y",
      annot (PArithmeticExpression.Variable("a"))
    )
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.Allocation("y", Variable("a")) :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << Exists v . y -> v ^ v -> 5+5 >> x := [y] << y -> 5+5 >> *)
let%test "precondition on x := [y], post-condition = << x -> y >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition =
    annot (PFormula.Allocation(
      "y",
      annot (PArithmeticExpression.Operation(
        PBinaryOperator.Plus,
            annot (PArithmeticExpression.Literal(5)),
            annot (PArithmeticExpression.Literal(5))
      ))
    )
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.Allocation("y", Literal(10)) :: [] in 
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << x -/> >> *)
let%test "precondition on x := [y], post-condition = << x -/> >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = annot ( PFormula.NonAllocated("x") ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << y -/> >> *)
let%test "precondition on x := [y], post-condition = << y -/> >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = annot ( PFormula.NonAllocated("y") ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << Exists v . y -> v >> x := [y] << Exists v . y -> v >> *)
let%test "precondition on x := [y], post-condition = << Exists v . y -> v >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition =
    annot ( PFormula.Exists("v",
      annot (PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("v"))
      )
    ))
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.Allocation("y", Variable("v")) :: [] in
  test_expected_bound_variables pre_condition 1 &&
  test_expected_disjoints pre_condition expected_disjoints ["v"]

(* << False >> x := [y] << Exists y . y -> v >> *)
let%test "precondition on x := [y], post-condition = << Exists v . y -> v >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition =
    annot ( PFormula.Exists("y",
      annot (PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("v"))
      )
    ))
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.False :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << False >> x := [y] << y -> x+2 >> *)
let%test "precondition on x := [y], post-condition = << y -> x+2 >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition =
    annot (PFormula.Allocation(
      "y",
      annot (PArithmeticExpression.Operation(
        PBinaryOperator.Plus,
            annot (PArithmeticExpression.Variable("x")),
            annot (PArithmeticExpression.Literal(2))
      ))
    )
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.False :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << y -> z+2 >> x := [y] << y -> z+2 >> *)
let%test "precondition on x := [y], post-condition = << y -> x+2 >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition =
    annot (PFormula.Allocation(
      "y",
      annot (PArithmeticExpression.Operation(
        PBinaryOperator.Plus,
            annot (PArithmeticExpression.Variable("z")),
            annot (PArithmeticExpression.Literal(2))
      ))
    )
  ) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.Allocation("y", Operation(Plus, Variable("z"), Literal(2))) :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << false >> *)
let%test "precondition on x := [y], post-condition = << false >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition = annot (PFormula.False) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.False :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << true >> x := [y] << true >> *)
let%test "precondition on x := [y], post-condition = << true >>" =
  let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
  let post_condition = annot (PFormula.True) in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.True :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << emp >> *)
let%test "precondition on x := [y], post-condition = << emp >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = annot ( PFormula.EmptyHeap ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << emp || emp >> *)
let%test "precondition on x := [y], post-condition = << emp || emp >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = 
  annot ( 
    PFormula.Or(
      annot (PFormula.EmptyHeap),
      annot (PFormula.EmptyHeap)
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << y -> a >> x := [y] << emp || y -> a >> *)
let%test "precondition on x := [y], post-condition = << emp || y -> a >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = 
  annot ( 
    PFormula.Or(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("a"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.Allocation("y", Variable("a")) :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << emp && y -> a >> *)
let%test "precondition on x := [y], post-condition = << emp && y -> a >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = 
  annot ( 
    PFormula.And(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("a"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(***************************** Frame Rule *********************************)

(* << y -> a >> x := [y] << emp * y -> a >> *)
let%test "precondition on x := [y], post-condition = << emp * y -> a >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("a"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.Allocation("y", Variable("a")) :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << False >> x := [y] << emp * y -> x >> *)
let%test "precondition on x := [y], post-condition = << emp * y -> x >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.Allocation(
        "y",
        annot (PArithmeticExpression.Variable("x"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << emp * y -/> >> *)
let%test "precondition on x := [y], post-condition = << emp * x -/> >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (
        PFormula.NonAllocated("y")
      )
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << y -> v * v -> 5 >> x := [y] << y -> v * v -> 5 >> *)
let%test "precondition on x := [y], post-condition = << y -> v * v -> 5 >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
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
        "v",
        annot (PArithmeticExpression.Literal(5))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(
    Formula.Allocation("y", Variable("v")),
    Formula.Allocation("v", Literal(5))
  )
   :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << False >> x := [y] << y -> v * x -> 5 >> *)
let%test "precondition on x := [y], post-condition = << y -> v * x -> 5 >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
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
        annot (PArithmeticExpression.Literal(5))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << Exists x . y -> 6 * x -> 6 >> x := [y] << Exists x . y -> 6 * x -> 5 >> *)
(*

(NormalizedFormulas.NormalForm.Formula.AndSeparately (
  (NormalizedFormulas.NormalForm.Formula.Allocation ("y",
    (NormalizedFormulas.NormalForm.ArithmeticExpression.Literal 6))
  ),
  (NormalizedFormulas.NormalForm.Formula.And (
    (NormalizedFormulas.NormalForm.Formula.Allocation ("1$x",
        (NormalizedFormulas.NormalForm.ArithmeticExpression.Literal 5))
    ),
    (NormalizedFormulas.NormalForm.Formula.Comparison (
      Annotation_logic.AnnotationLogic.BinaryComparison.Equals,
      (NormalizedFormulas.NormalForm.ArithmeticExpression.Variable "1$x"),
      (NormalizedFormulas.NormalForm.ArithmeticExpression.Literal 6))
    ))
  ))
)
y -> 6 * 1$x -> 5 && 1$x = 6


let%test "precondition on x := [y], post-condition = << Exists x . y -> 6 * x -> 5 >>" =
let command = annot_cmd (Commands.HeapAtomicCommand.ReadHeap("x","y")) in
let post_condition = 
  annot ( PFormula.Exists("x", 
    annot ( 
      PFormula.AndSeparately(
        annot (
          PFormula.Allocation(
          "y",
          annot (PArithmeticExpression.Literal(6))
        )),
        annot (
          PFormula.Allocation(
          "x",
          annot (PArithmeticExpression.Literal(5))
        ))
      )
    )
  )) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
print_formulas pre_condition.disjoints;
let expected_disjoints = 
  Formula.AndSeparately(
    Formula.Allocation("y", Literal(6)),
    Formula.Allocation("x", Literal(6))
  )
   :: [] in
test_expected_bound_variables pre_condition 1 &&
test_expected_disjoints pre_condition expected_disjoints ["x"]

(*
(* << false >> x := [y] << x -/> * x -> v >> *)
let%test "precondition on x := [y], post-condition = << x -/> * x -> v >>" =
let command = annot_cmd (
  Commands.HeapAtomicCommand.ReadHeap("x","y")
    (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
  )
) in
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
test_expected_disjoints pre_condition expected_disjoints []
*)
(* << false >> x := [y] << x -/> * x -> y >> *)
let%test "precondition on x := [y], post-condition = << x -/> * x -> v >>" =
let command = annot_cmd (
  Commands.HeapAtomicCommand.ReadHeap("x","y")
    (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
  )
) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.NonAllocated("x")),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("y"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.False :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << true >> x := [y] << emp * true >> *)
let%test "precondition on x := [y], post-condition = << emp * true >>" =
let command = annot_cmd (
  Commands.HeapAtomicCommand.ReadHeap("x","y")
    (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
  )
) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.EmptyHeap),
      annot (PFormula.True)
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = Formula.True :: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << Exists v . true * x -> v >> x := [y] << true * x -> y >> *)
let%test "precondition on x := [y], post-condition = << true * x -> y >>" =
let command = annot_cmd (
  Commands.HeapAtomicCommand.ReadHeap("x","y")
    (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
  )
) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.True),
      annot (
        PFormula.Allocation(
        "x",
        annot (PArithmeticExpression.Variable("y"))
      ))
    )
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(
    Formula.True,
    Formula.Allocation("x", Variable("v"))
  )
  :: [] in
test_expected_bound_variables pre_condition 1 &&
test_expected_disjoints pre_condition expected_disjoints ["v"]

(* << true * y -> v >> x := [y] << true * y -> v >> *)
let%test "precondition on x := [y], post-condition = << true * y -> v >>" =
let command = annot_cmd (
  Commands.HeapAtomicCommand.ReadHeap("x","y")
    (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
  )
) in
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
test_expected_disjoints pre_condition expected_disjoints []

(* << false >> x := [y] << v = 5 * x -> v * y -> v >> *)
let%test "precondition on x := [y], post-condition = << v = 5 * x -> v * y -> v >>" =
let command = annot_cmd (
  Commands.HeapAtomicCommand.ReadHeap("x","y")
    (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
  )
) in
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
  Formula.False:: [] in
test_expected_bound_variables pre_condition 0 &&
test_expected_disjoints pre_condition expected_disjoints []

(* << Exists w . x -> w * v = 5 * y -> v >> x := [y] << v = 5 * x -> y * y -> v >> *)
let%test "precondition on x := [y], post-condition = << v = 5 * x -> y * y -> v >>" =
let command = annot_cmd (
  Commands.HeapAtomicCommand.ReadHeap("x","y")
    (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
  )
) in
let post_condition = 
  annot ( 
    PFormula.AndSeparately(
      annot (PFormula.Allocation("v", (annot (PArithmeticExpression.Literal(5) ) ) ) ),
      annot (PFormula.AndSeparately(
        annot (PFormula.Allocation("x", annot (PArithmeticExpression.Variable("y") ) ) ),
        annot (PFormula.Allocation("y", annot (PArithmeticExpression.Variable("v") ) ) ) 
      )
    ))
  ) in
let post_condition = existential_disjuntive_normal_form post_condition in
let pre_condition = compute_precondition command post_condition in
let expected_disjoints = 
  Formula.AndSeparately(
    Formula.Allocation("v", Literal(5)),
    Formula.AndSeparately(
      Formula.Allocation("x", Variable("w")),
      Formula.Allocation("y", Variable("v"))
    )
  ) :: [] in
test_expected_bound_variables pre_condition 1 &&
test_expected_disjoints pre_condition expected_disjoints ["w"]

(* << false >> x := [y] << Exists x . x -> y * v = 5 >> *)
let%test "precondition on x := [y], post-condition = << Exists x . x -> y * v = 5 >>" =
  let command = annot_cmd (
    Commands.HeapAtomicCommand.ReadHeap("x","y")
      (annot_cmd (Commands.ArithmeticExpression.Variable("y")))
    )
  ) in
  let post_condition =
    annot (PFormula.AndSeparately(
      annot (PFormula.Exists("x", 
        annot (PFormula.Allocation(
          "x",
          annot (PArithmeticExpression.Variable("y"))
          )
        )
      ) ),
      annot (PFormula.Allocation("v", (annot (PArithmeticExpression.Literal(5) ) ) ) )
    ) )
  in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.False :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

*)