open NormalizationBase
open DataStructures.Parser.LogicFormulas

open Analysis_TestCommon

let%test "existentialized non bound variable" =
  let formula = annot (
    Formula.Exists("x", annot (
      Formula.Exists("y", annot (
        Formula.NonAllocated("x")
      ))
    ))
  )
  in
  let normalized = existential_disjuntive_normal_form formula 0 in
  test_expected_free_variables normalized ("x"::[])

let%test "disjoint merging" =
  let formula = annot (
    Formula.Or(annot(
      Formula.Or(annot (
        Formula.NonAllocated("x")
      ), annot (
        Formula.NonAllocated("y")
      ))
    ), annot (Formula.Or(annot (
        Formula.NonAllocated("z")
      ), annot (
        Formula.NonAllocated("w")
      ))
    ))
  ) in
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_disjoints = 
    annot_unit (Formula.NonAllocated("x")) :: 
    annot_unit (Formula.NonAllocated("y")) :: 
    annot_unit (Formula.NonAllocated("z")) :: 
    annot_unit (Formula.NonAllocated("w")) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "and distribution" =
  let formula = annot (
    Formula.And(annot(
      Formula.Or(annot (
        Formula.NonAllocated("x")
      ), annot (
        Formula.NonAllocated("y")
      ))
    ), annot (Formula.Or(annot (
        Formula.NonAllocated("z")
      ), annot (
        Formula.NonAllocated("w")
      ))
    ))
  ) in 
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_disjoints = annot_unit ( Formula.And(
    annot_unit (Formula.NonAllocated("x")),
    annot_unit (Formula.NonAllocated("z"))
  )) :: annot_unit ( Formula.And(
    annot_unit (Formula.NonAllocated("x")),
    annot_unit (Formula.NonAllocated("w"))
  )) :: annot_unit ( Formula.And(
    annot_unit (Formula.NonAllocated("y")),
    annot_unit (Formula.NonAllocated("z"))
  )) :: annot_unit ( Formula.And(
    annot_unit (Formula.NonAllocated("y")),
    annot_unit (Formula.NonAllocated("w"))
  )) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "and separately distribution" =
  let formula = annot (
    Formula.AndSeparately(annot(
      Formula.Or(annot (
        Formula.NonAllocated("x")
      ), annot (
        Formula.NonAllocated("y")
      ))
    ), annot (Formula.Or(annot (
        Formula.NonAllocated("z")
      ), annot (
        Formula.NonAllocated("w")
      ))
    ))
  ) in 
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_disjoints = annot_unit ( Formula.AndSeparately(
    annot_unit (Formula.NonAllocated("x")),
    annot_unit (Formula.NonAllocated("z"))
  )) :: annot_unit ( Formula.AndSeparately(
    annot_unit (Formula.NonAllocated("x")),
    annot_unit (Formula.NonAllocated("w"))
  )) :: annot_unit ( Formula.AndSeparately(
    annot_unit (Formula.NonAllocated("y")),
    annot_unit (Formula.NonAllocated("z"))
  )) :: annot_unit ( Formula.AndSeparately(
    annot_unit (Formula.NonAllocated("y")),
    annot_unit (Formula.NonAllocated("w"))
  )) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "variables renaming when merging normalized forms" =
  let formula = annot (
    Formula.AndSeparately(
      annot (Formula.Exists("y",
        annot (Formula.Exists("x", 
          annot (Formula.And(
            annot (Formula.NonAllocated("x")), 
            annot (Formula.NonAllocated("y"))
          ))
        ))
      )),
      annot (Formula.Or(
        annot (Formula.Exists("x",
          annot (Formula.NonAllocated("x"))
        )),
        annot (Formula.NonAllocated("y"))
      ))
    )
  ) in 
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_identifiers = "x" :: "0$y" :: "1$x" :: [] in
  let expected_disjoints =
    annot_unit (Formula.AndSeparately(
      annot_unit (Formula.And(
        annot_unit (Formula.NonAllocated("x")),
        annot_unit (Formula.NonAllocated("0$y"))
      )),
      annot_unit (Formula.NonAllocated("1$x"))
    )) ::
    annot_unit (Formula.AndSeparately(
      annot_unit (Formula.And(
        annot_unit (Formula.NonAllocated("x")),
        annot_unit (Formula.NonAllocated("0$y"))
      )),
      annot_unit (Formula.NonAllocated("y"))
    )) :: []
  in
  test_expected_free_variables normalized expected_identifiers &&
  test_expected_disjoints normalized expected_disjoints