open Ast.HeapRegularCommands
open Cfg__node

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

  let rec simplify(root: 'a HeapAtomicCommand.t list Node.t) : unit =
    match Node.get_succ root with
    | [] -> ()
    | [a] when Node.get_id a != Node.get_id root -> (
      match Node.get_pred a with
      | [] -> raise (Invalid_argument "Your graph is wrong")
      | [_] ->
         Node.set_exp root ((Node.get_exp root) @ (Node.get_exp a));
         Node.set_succ root (Node.get_succ a)
      | _ -> simplify a
    )
    | [_] -> ()
    | a -> List.iter simplify a

  let convert (root: 'a HeapRegularCommand.t) : 'a HeapAtomicCommand.t list Node.t =
    match convert_helper(root) with
    | (a, _) -> simplify a; a
end
