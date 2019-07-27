open Alcotest
open Calc_solver

let op_test = Alcotest.(check int) ""
let operations_tests = [
  "Delete last digit", `Quick, (fun _ -> op_test 123 (del_last 1234));
  "Inverse 10", `Quick, (fun _ -> op_test (inv10 987) 123);
  "Replace", `Quick, (fun _ -> op_test (num_replace 3004020 "00" "12") 3124020)
]

(* TODO understand how the pretty printer works *)
let operation_t = Alcotest.testable (fun a b -> ()) (=)

let lvl_test = Alcotest.check (Alcotest.list operation_t) ""
let level134 () = lvl_test
    [MetaInc 1; Append 3; Append 4]
    (solve 0 34 3 [Append 2; Append 3; MetaInc 1] None)
let level155 () = lvl_test
    [StoreSave; Div 3; StoreUse; StoreSave; StoreUse; Replace ("39", "93"); Replace ("39", "93"); Div 3; Replace ("31", "00")]
    (solve 9 3001 9 [Replace("39", "93"); StoreSave; StoreUse; Div 3; Replace("31", "00")] None)
let level168 () = lvl_test
    [Append 2; Sum; Inv10; Append 2; Sum; Inv10]
    (solve 26 99 6 [Sum; Inv10; Append 2] None)
let level185 () = lvl_test
    [Append 7; Append 6; Div 2; Append 7]
    (solve 525 150 5 [Add 1; Append 6; Append 7; Div 2] (Some (portal 0 3)))
let level188 () = lvl_test
    [Append 5; Mirror; StoreSave; StoreUse]
    (solve 25 822 6 [Mirror; Append 5; StoreUse; StoreSave; Del] (Some (portal 1 3)))
let level199 () = lvl_test
    [Append 7; Replace ("3", "5"); Append 7; Shift 1; Inv10; Append 7]
    (solve 3002 3507 6 [Append 7; Replace("3","5"); Inv10; Shift 1] (Some (portal 0 4)))

let solve_tests = [
  "Level 134", `Quick, level134;
  "Level 155", `Quick, level155;
  "Level 168", `Quick, level168;
  "Level 185", `Quick, level185;
  "Level 188", `Quick, level188;
  "Level 199", `Quick, level199;
]

(* Run it *)
let () =
  Alcotest.run "CalcSolverTests" [
    "Operation tests", operations_tests;
    "Solve Tests", solve_tests;
  ]
