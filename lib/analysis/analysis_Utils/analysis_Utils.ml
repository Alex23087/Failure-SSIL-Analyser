include RenameVariable
include NormalFormUtils
include CommandToLogicConversion

open DataStructures.Analysis.NormalForm

let annotate = DataStructures.AnnotatedNode.make

(* https://stackoverflow.com/a/10893700 *)
let list_cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)
