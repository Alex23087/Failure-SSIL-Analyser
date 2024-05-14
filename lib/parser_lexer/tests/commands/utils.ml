open Prelude.Ast
open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas

let dummy_position = { line = -1; column = -1 }

let rec remove_position_from_annotations (command: Commands.t): Commands.t =
  let newAnnot = {
    position = dummy_position;
    logic_formula = match command.annotation.logic_formula with
      | None -> None
      | Some f -> Some (remove_position_from_annotations_formulae f)
  } in
  match command.node with
  | HeapRegularCommand.Star c -> Commands.annotate (HeapRegularCommand.Star (remove_position_from_annotations c)) newAnnot
  | HeapRegularCommand.Sequence (c1, c2) -> Commands.annotate (HeapRegularCommand.Sequence ((remove_position_from_annotations c1), (remove_position_from_annotations c2))) newAnnot
  | HeapRegularCommand.NondeterministicChoice (c1, c2) -> Commands.annotate (HeapRegularCommand.NondeterministicChoice ((remove_position_from_annotations c1), (remove_position_from_annotations c2))) newAnnot
  | HeapRegularCommand.Command c -> Commands.annotate (HeapRegularCommand.Command (remove_position_from_annotations_atomic c)) newAnnot

and remove_position_from_annotations_atomic (atomic_command: HeapAtomicCommand.t): HeapAtomicCommand.t =
  let newAnnot = {
    position = dummy_position;
    logic_formula = match atomic_command.annotation.logic_formula with
      | None -> None
      | Some f -> Some (remove_position_from_annotations_formulae f)
  } in
  match atomic_command.node with
  | HeapAtomicCommand.Skip -> Commands.annotate HeapAtomicCommand.Skip newAnnot
  | HeapAtomicCommand.Allocation id -> Commands.annotate (HeapAtomicCommand.Allocation id) newAnnot
  | HeapAtomicCommand.Free id -> Commands.annotate (HeapAtomicCommand.Free id) newAnnot
  | HeapAtomicCommand.Assignment (id, ae) -> Commands.annotate (HeapAtomicCommand.Assignment (id, remove_position_from_annotations_aexp ae)) newAnnot
  | HeapAtomicCommand.Guard be -> Commands.annotate (HeapAtomicCommand.Guard be) newAnnot
  | HeapAtomicCommand.NonDet id -> Commands.annotate (HeapAtomicCommand.NonDet id) newAnnot
  | HeapAtomicCommand.ReadHeap (id1,id2) -> Commands.annotate (HeapAtomicCommand.ReadHeap (id1,id2)) newAnnot
  | HeapAtomicCommand.WriteHeap (id1, ae) -> Commands.annotate (HeapAtomicCommand.WriteHeap (id1, remove_position_from_annotations_aexp ae)) newAnnot

and remove_position_from_annotations_aexp (aexp: Commands.ArithmeticExpression.t): Commands.ArithmeticExpression.t =
  let newAnnot = {
    position = dummy_position;
    logic_formula = match aexp.annotation.logic_formula with
      | None -> None
      | Some f -> Some (remove_position_from_annotations_formulae f)
  } in
  match aexp.node with
  | Commands.ArithmeticExpression.Variable id -> Commands.annotate (Commands.ArithmeticExpression.Variable id) newAnnot
  | Commands.ArithmeticExpression.Literal i -> Commands.annotate (Commands.ArithmeticExpression.Literal i) newAnnot
  | Commands.ArithmeticExpression.BinaryOperation (aop, ae1, ae2) -> Commands.annotate (Commands.ArithmeticExpression.BinaryOperation (aop, (remove_position_from_annotations_aexp ae1), (remove_position_from_annotations_aexp ae2))) newAnnot

and remove_position_from_annotations_bexp (bexp: Commands.BooleanExpression.t): Commands.BooleanExpression.t =
  let newAnnot = {
    position = dummy_position;
    logic_formula = match bexp.annotation.logic_formula with
      | None -> None
      | Some f -> Some (remove_position_from_annotations_formulae f)
  } in
  match bexp.node with
  | BooleanExpression.True -> Commands.annotate BooleanExpression.True newAnnot
  | BooleanExpression.False -> Commands.annotate BooleanExpression.False newAnnot
  | BooleanExpression.Not be -> Commands.annotate (BooleanExpression.Not (remove_position_from_annotations_bexp be)) newAnnot
  | BooleanExpression.And (be1, be2) -> Commands.annotate (BooleanExpression.And ((remove_position_from_annotations_bexp be1), (remove_position_from_annotations_bexp be2))) newAnnot
  | BooleanExpression.Or (be1, be2) -> Commands.annotate (BooleanExpression.Or ((remove_position_from_annotations_bexp be1), (remove_position_from_annotations_bexp be2))) newAnnot
  | BooleanExpression.Comparison (bc, ae1, ae2) -> Commands.annotate (BooleanExpression.Comparison (bc, (remove_position_from_annotations_aexp ae1), (remove_position_from_annotations_aexp ae2))) newAnnot
and remove_position_from_annotations_formulae (formula: Formula.t): Formula.t =
  let newAnnot = {
    position = dummy_position;
  } in
  match formula.node with
  | Formula.True -> LogicFormulas.annotate Formula.True newAnnot
  | Formula.False -> LogicFormulas.annotate Formula.False newAnnot
  | Formula.EmptyHeap -> LogicFormulas.annotate Formula.EmptyHeap newAnnot
  | Formula.NonAllocated id -> LogicFormulas.annotate (Formula.NonAllocated id) newAnnot
  | Formula.Or (f1, f2) -> LogicFormulas.annotate (Formula.Or ((remove_position_from_annotations_formulae f1), (remove_position_from_annotations_formulae f2))) newAnnot
  | Formula.And (f1, f2) -> LogicFormulas.annotate (Formula.And ((remove_position_from_annotations_formulae f1), (remove_position_from_annotations_formulae f2))) newAnnot
  | Formula.Exists (id, f) -> LogicFormulas.annotate (Formula.Exists (id, (remove_position_from_annotations_formulae f))) newAnnot
  | Formula.AndSeparately (f1, f2) -> LogicFormulas.annotate (Formula.AndSeparately ((remove_position_from_annotations_formulae f1), (remove_position_from_annotations_formulae f2))) newAnnot
  | Formula.Comparison (bc, ae1, ae2) -> LogicFormulas.annotate (Formula.Comparison (bc, (remove_position_from_annotations_formulae_aexp ae1), (remove_position_from_annotations_formulae_aexp ae2))) newAnnot
  | Formula.Allocation (id, ae) -> LogicFormulas.annotate (Formula.Allocation (id, (remove_position_from_annotations_formulae_aexp ae))) newAnnot
and remove_position_from_annotations_formulae_aexp (aexp: LogicFormulas.ArithmeticExpression.t): LogicFormulas.ArithmeticExpression.t =
  let newAnnot = {
    position = dummy_position;
  } in
  match aexp.node with
  | LogicFormulas.ArithmeticExpression.Variable id -> LogicFormulas.annotate (LogicFormulas.ArithmeticExpression.Variable id) newAnnot
  | LogicFormulas.ArithmeticExpression.Literal i -> LogicFormulas.annotate (LogicFormulas.ArithmeticExpression.Literal i) newAnnot
  | LogicFormulas.ArithmeticExpression.Operation (op, ae1, ae2) -> LogicFormulas.annotate (LogicFormulas.ArithmeticExpression.Operation (op, (remove_position_from_annotations_formulae_aexp ae1), (remove_position_from_annotations_formulae_aexp ae2))) newAnnot
