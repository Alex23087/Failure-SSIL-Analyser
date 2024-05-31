open Ast.HeapRegularCommands
open Base

(* let rec convert_helper(root: 'a HeapRegularCommand.t) = *)
(*   let toret = match root.node with *)
(*   | Command(atom) -> *)
(*      print_endline "Command"; *)
(*      [Node.make [atom] [] []] *)
(*   | Sequence(comm1, comm2) -> *)
(*      print_endline "Sequence"; *)
(*      (match (convert_helper comm1, convert_helper comm2) with *)
(*      | ([a], [b]) -> *)
(*         Node.concat a b; *)
(*         [a] *)
(*      | (aas, bbs) -> *)
(*         let dummymiddle = Node.make [] [] [] in *)
(*         List.iter (fun x -> Node.concat x dummymiddle) aas; *)
(*         List.iter (fun y -> Node.concat dummymiddle y) bbs; *)
(*         (\* List.iter (fun x -> List.iter (fun y -> Node.concat x y) bbs) aas; *\) *)
(*         aas) *)
(*   | NondeterministicChoice(comm1, comm2) -> *)
(*      print_endline "NonDet Choice"; *)
(*      ( convert_helper comm1 ) @ ( convert_helper comm2 ) *)
(*   | Star(comm) -> *)
(*      print_endline "Star"; *)
(*      (match convert_helper comm with *)
(*       | [a] -> *)
(*          Node.concat a a; *)
(*          [a] *)
(*       | aas -> *)
(*          let dummyentry = Node.make [] [] [] in *)
(*          let dummyexit = Node.make [] [] [] in *)
(*          List.iter *)
(*            (fun x -> Node.concat dummyentry x; *)
(*                      Node.concat x dummyexit) *)
(*            aas; *)
(*          Node.concat dummyexit dummyentry; *)
(*          [dummyentry] *)
(*      ) in *)
(*   List.iter (fun x -> print_endline (Node.show (fun _ _ -> ()) x)) toret; *)
(*   toret *)

let rec convert_helper(root: 'a HeapRegularCommand.t) :
          'a HeapAtomicCommand.t list Node.t *
            'a HeapAtomicCommand.t list Node.t ref =
  match root.node with
  | Command(atom) ->
     print_endline "Command";
     let newcommand = Node.make [atom] [] [] in
     (newcommand, ref newcommand)
  | Sequence(comm1, comm2) ->
     print_endline "Sequence";
     let ((root1, end1), (root2, end2)) = (convert_helper comm1, convert_helper comm2) in
     Node.addsucc !end1 root2;
     (root1, end2)
  | NondeterministicChoice(comm1, comm2) ->
     print_endline "NonDet Choice";
     let ((root1, end1), (root2, end2)) = (convert_helper comm1, convert_helper comm2) in
     let dummyentry = Node.make [] [] [] in
     let dummyexit  = Node.make [] [] [] in
     Node.addsucc dummyentry root1;
     Node.addsucc dummyentry root2;
     Node.addsucc !end1 dummyexit;
     Node.addsucc !end2 dummyexit;
     (dummyentry, ref dummyexit)
  | Star(comm) ->
     print_endline "Star";
     let (root1, end1) = convert_helper comm in
     Node.addsucc !end1 root1;
     (root1, end1)


let convert(root: 'a HeapRegularCommand.t) : 'a HeapAtomicCommand.t list Node.t =
  (* match convert_helper(root) with *)
  (* | [a] -> a *)
  (* | aas -> *)
  (*    let entry = Node.make [] [] [] in *)
  (*    List.iter (fun x -> Node.concat entry x) aas; *)
  (*    entry *)
  match convert_helper(root) with
  | (a, _) -> a
