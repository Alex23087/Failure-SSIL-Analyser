open Analysis.DataStructures.Parser
open Analysis.DataStructures
open Parsing
let dummy_position = { line = -1; column = -1 }

let rec remove_position_from_annotations (command: Commands.t) =
  let logic_formula = match command.annotation.logic_formula with
    | None -> None
    | Some f -> None (* Some (remove_position_from_annotations_formulae f) *) in
  let newAnnot = Commands.make_annotation_position dummy_position logic_formula in
  match command.node with
  (* todo questo è diventato un atomic command *)
  (* | Commands.HeapRegularCommand. -> Commands.annotate Commands.Skip newAnnot *)
  | Commands.HeapRegularCommand.Star c -> AnnotatedNode.make (Commands.HeapRegularCommand.Star (remove_position_from_annotations c)) newAnnot
  | Commands.HeapRegularCommand.Sequence (c1, c2) -> AnnotatedNode.make (Commands.HeapRegularCommand.Sequence ((remove_position_from_annotations c1), (remove_position_from_annotations c2))) newAnnot
  | Commands.HeapRegularCommand.NondeterministicChoice (c1, c2) -> AnnotatedNode.make (Commands.HeapRegularCommand.NondeterministicChoice ((remove_position_from_annotations c1), (remove_position_from_annotations c2))) newAnnot
  | Commands.HeapRegularCommand.Command c -> AnnotatedNode.make (Commands.HeapRegularCommand.Command (remove_position_from_annotations_atomic c)) newAnnot

(* and remove_position_from_annotations_atomic (atomic_command: Commands.atomic_t): Commands.atomic_t =
  let newAnnot = {
    Prelude.Ast.position = dummy_position;
    logic_formula = match atomic_command.annotation.logic_formula with
      | None -> None
      | Some f -> Some (remove_position_from_annotations_formulae f)
  } in
  match atomic_command.node with
  | HeapAtomicCommand.Skip -> Prelude.Ast.Commands.annotate HeapAtomicCommand.Skip newAnnot
  | HeapAtomicCommand.Allocation id -> Prelude.Ast.Commands.annotate (HeapAtomicCommand.Allocation id) newAnnot
  | HeapAtomicCommand.Free id -> Prelude.Ast.Commands.annotate (HeapAtomicCommand.Free id) newAnnot
  | HeapAtomicCommand.Assignment (id, ae) -> Prelude.Ast.Commands.annotate (HeapAtomicCommand.Assignment (id, remove_position_from_annotations_aexp ae)) newAnnot
  | HeapAtomicCommand.Guard be -> Prelude.Ast.Commands.annotate (HeapAtomicCommand.Guard (remove_position_from_annotations_bexp be)) newAnnot
  | HeapAtomicCommand.NonDet id -> Prelude.Ast.Commands.annotate (HeapAtomicCommand.NonDet id) newAnnot
  | HeapAtomicCommand.ReadHeap (id1,id2) -> Prelude.Ast.Commands.annotate (HeapAtomicCommand.ReadHeap (id1,id2)) newAnnot
  | HeapAtomicCommand.WriteHeap (id1, ae) -> Prelude.Ast.Commands.annotate (HeapAtomicCommand.WriteHeap (id1, remove_position_from_annotations_aexp ae)) newAnnot

and remove_position_from_annotations_aexp (aexp: Commands.arithmetic_t): Commands.arithmetic_t =
  let newAnnot = {
    Prelude.Ast.position = dummy_position;
    logic_formula = match aexp.annotation.logic_formula with
      | None -> None
      | Some f -> Some (remove_position_from_annotations_formulae f)
  } in
  match aexp.node with
  | 
  | Prelude.Ast.Commands.ArithmeticExpression.Variable id -> Prelude.Ast.Commands.annotate (Prelude.Ast.Commands.ArithmeticExpression.Variable id) newAnnot
  | Prelude.Ast.Commands.ArithmeticExpression.Literal i -> Prelude.Ast.Commands.annotate (Prelude.Ast.Commands.ArithmeticExpression.Literal i) newAnnot
  | Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (aop, ae1, ae2) -> Prelude.Ast.Commands.annotate (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (aop, (remove_position_from_annotations_aexp ae1), (remove_position_from_annotations_aexp ae2))) newAnnot

and remove_position_from_annotations_bexp (bexp: Commands.boolean_t): Commands.boolean_t =
  let newAnnot = {
    Prelude.Ast.position = dummy_position;
    logic_formula = match bexp.annotation.logic_formula with
      | None -> None
      | Some f -> Some (remove_position_from_annotations_formulae f)
  } in
  match bexp.node with
  | BooleanExpression.True -> Prelude.Ast.Commands.annotate BooleanExpression.True newAnnot
  | BooleanExpression.False -> Prelude.Ast.Commands.annotate BooleanExpression.False newAnnot
  | BooleanExpression.Not be -> Prelude.Ast.Commands.annotate (BooleanExpression.Not (remove_position_from_annotations_bexp be)) newAnnot
  | BooleanExpression.And (be1, be2) -> Prelude.Ast.Commands.annotate (BooleanExpression.And ((remove_position_from_annotations_bexp be1), (remove_position_from_annotations_bexp be2))) newAnnot
  | BooleanExpression.Or (be1, be2) -> Prelude.Ast.Commands.annotate (BooleanExpression.Or ((remove_position_from_annotations_bexp be1), (remove_position_from_annotations_bexp be2))) newAnnot
  | BooleanExpression.Comparison (bc, ae1, ae2) -> Prelude.Ast.Commands.annotate (BooleanExpression.Comparison (bc, (remove_position_from_annotations_aexp ae1), (remove_position_from_annotations_aexp ae2))) newAnnot
and remove_position_from_annotations_formulae (formula: Formula.t): Formula.t =
  let newAnnot = {
    Prelude.Ast.position = dummy_position;
  } in
  match formula.node with
  | Formula.True -> Prelude.Ast.LogicFormulas.annotate Formula.True newAnnot
  | Formula.False -> Prelude.Ast.LogicFormulas.annotate Formula.False newAnnot
  | Formula.EmptyHeap -> Prelude.Ast.LogicFormulas.annotate Formula.EmptyHeap newAnnot
  | Formula.NonAllocated id -> Prelude.Ast.LogicFormulas.annotate (Formula.NonAllocated id) newAnnot
  | Formula.Or (f1, f2) -> Prelude.Ast.LogicFormulas.annotate (Formula.Or ((remove_position_from_annotations_formulae f1), (remove_position_from_annotations_formulae f2))) newAnnot
  | Formula.And (f1, f2) -> Prelude.Ast.LogicFormulas.annotate (Formula.And ((remove_position_from_annotations_formulae f1), (remove_position_from_annotations_formulae f2))) newAnnot
  | Formula.Exists (id, f) -> Prelude.Ast.LogicFormulas.annotate (Formula.Exists (id, (remove_position_from_annotations_formulae f))) newAnnot
  | Formula.AndSeparately (f1, f2) -> Prelude.Ast.LogicFormulas.annotate (Formula.AndSeparately ((remove_position_from_annotations_formulae f1), (remove_position_from_annotations_formulae f2))) newAnnot
  | Formula.Comparison (bc, ae1, ae2) -> Prelude.Ast.LogicFormulas.annotate (Formula.Comparison (bc, (remove_position_from_annotations_formulae_aexp ae1), (remove_position_from_annotations_formulae_aexp ae2))) newAnnot
  | Formula.Allocation (id, ae) -> Prelude.Ast.LogicFormulas.annotate (Formula.Allocation (id, (remove_position_from_annotations_formulae_aexp ae))) newAnnot
and remove_position_from_annotations_formulae_aexp (aexp: Prelude.Ast.LogicFormulas.ArithmeticExpression.t): Prelude.Ast.LogicFormulas.ArithmeticExpression.t = 
  let newAnnot = {
    Prelude.Ast.position = dummy_position;
  } in
  match aexp.node with
  | Prelude.Ast.LogicFormulas.ArithmeticExpression.Variable id -> Prelude.Ast.LogicFormulas.annotate (Prelude.Ast.LogicFormulas.ArithmeticExpression.Variable id) newAnnot
  | Prelude.Ast.LogicFormulas.ArithmeticExpression.Literal i -> Prelude.Ast.LogicFormulas.annotate (Prelude.Ast.LogicFormulas.ArithmeticExpression.Literal i) newAnnot
  | Prelude.Ast.LogicFormulas.ArithmeticExpression.Operation (op, ae1, ae2) -> Prelude.Ast.LogicFormulas.annotate (Prelude.Ast.LogicFormulas.ArithmeticExpression.Operation (op, (remove_position_from_annotations_formulae_aexp ae1), (remove_position_from_annotations_formulae_aexp ae2))) newAnnot *)


(* let parse_command source = let lexbuf = Lexing.from_string ~with_positions:true source in
              let ast = parse Lexer.lex lexbuf in
                match Either.find_left ast with
                | Some command -> remove_position_from_annotations command
                | None -> raise (Failure "not a command");;

let parse_formula source = let lexbuf = Lexing.from_string ~with_positions:true source in
              let ast = parse Lexer.lex lexbuf in
                match Either.find_right ast with
                | Some formula -> remove_position_from_annotations_formulae formula
                | None -> raise (Failure "not a formula");; *)