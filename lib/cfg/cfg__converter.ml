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
       Node.addsucc !end1 root2;
       (root1, end2)
    | NondeterministicChoice(comm1, comm2) ->
       let ((root1, end1), (root2, end2)) = (convert_helper comm1, convert_helper comm2) in
       let dummyentry = Node.make [] [] [] in
       let dummyexit  = Node.make [] [] [] in
       Node.addsucc dummyentry root1;
       Node.addsucc dummyentry root2;
       Node.addsucc !end1 dummyexit;
       Node.addsucc !end2 dummyexit;
       (dummyentry, ref dummyexit)
    | Star(comm) ->
       let (root1, end1) = convert_helper comm in
       Node.addsucc !end1 root1;
       (root1, end1)

  let rec simplify(root: 'a HeapAtomicCommand.t list Node.t) : unit =
    match Node.succ root with
    | [] -> ()
    | [a] when Node.getnodeid a != Node.getnodeid root -> (
      match Node.prev a with
      | [] -> raise (Invalid_argument "Your graph is wrong")
      | [_] ->
         Node.replaceexp root ((Node.getexp root) @ (Node.getexp a));
         Node.setsucc root (Node.succ a)
      | _ -> simplify a
    )
    | [_] -> ()
    | a -> List.iter simplify a

  let convert (root: 'a HeapRegularCommand.t) : 'a HeapAtomicCommand.t list Node.t =
    match convert_helper(root) with
    | (a, _) -> simplify a; a
end
