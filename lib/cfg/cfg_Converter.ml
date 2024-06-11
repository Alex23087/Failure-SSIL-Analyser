open Ast.HeapRegularCommands
open Cfg_Node

module Converter = struct
  let rec convert_helper(root: 'a HeapRegularCommand.t) :
            'a HeapAtomicCommand.t list Node.t *
              'a HeapAtomicCommand.t list Node.t ref =
    match root.node with
    | Command(atom) ->
       let newcommand = Node.make [atom] [] [] in
       (newcommand, ref newcommand)
    | Sequence(comm1, comm2) ->
       let ((root1, end1), (root2, end2)) = (convert_helper comm1, convert_helper comm2) in
       Node.add_succ !end1 root2;
       (root1, end2)
    | NondeterministicChoice(comm1, comm2) ->
       let ((root1, end1), (root2, end2)) = (convert_helper comm1, convert_helper comm2) in
       let dummyentry = Node.make [] [] [] in
       let dummyexit  = Node.make [] [] [] in
       Node.add_succ dummyentry root1;
       Node.add_succ dummyentry root2;
       Node.add_succ !end1 dummyexit;
       Node.add_succ !end2 dummyexit;
       (dummyentry, ref dummyexit)
    | Star(comm) ->
       let (root1, end1) = convert_helper comm in
       Node.add_succ !end1 root1;
       (root1, end1)

  let simplify (keep_structure: bool) (root: 'a HeapAtomicCommand.t list Node.t) : unit =
    let alreadyvisited = ref [] in

    let rec apply_and_recurse (root) (a) (keep_structure) : unit =
      Node.set_exp root ((Node.get_exp root) @ (Node.get_exp a));
      Node.set_succ root (Node.get_succ a);
      help_simplify keep_structure root
    and help_simplify (keep_structure: bool) (root: 'a HeapAtomicCommand.t list Node.t) : unit =
      if List.mem (Node.get_id root) !alreadyvisited then
        ()
      else (
        alreadyvisited := (Node.get_id root) :: !alreadyvisited;
        match Node.get_succ root with
        | [] -> ()
        | [a] when Node.get_id a != Node.get_id root -> (
          match Node.get_pred a with
          | [] -> raise (Invalid_argument "Your graph is wrong")
          | [_] -> (
            match (keep_structure, Node.get_exp root, Node.get_exp a) with
            | (true, root_exp::_, a_exp::_) -> (
              match (root_exp.node, a_exp.node) with
              | (Skip, Skip)
              | (Assignment(_, _), Assignment(_, _))
              | (NonDet(_), NonDet(_))
              | (Guard(_), Guard(_))
              | (Allocation(_), Allocation(_))
              | (Free(_), Free(_))
              | (ReadHeap(_, _), ReadHeap(_, _))
              | (WriteHeap(_, _), WriteHeap(_, _)) -> (apply_and_recurse root a keep_structure)
              | _ -> help_simplify keep_structure a
            )
            | (true, _, _) -> (
              help_simplify keep_structure a
            )
            | (false, _, _) -> (
              apply_and_recurse root a keep_structure
            )
          )
          | _ -> help_simplify keep_structure a
        )
        | [a] when Node.get_id a = Node.get_id root -> ()
        | a -> List.iter (help_simplify keep_structure) a
      )
    in
    help_simplify keep_structure root

  let convert ?(keep_structure: bool = true) (root: 'a HeapRegularCommand.t) : 'a HeapAtomicCommand.t list Node.t =
    match convert_helper(root) with
    | (a, _) -> simplify keep_structure a; a
end
