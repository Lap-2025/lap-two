open OUnit2
open Petrinets

let canary_test = "testing tests" >:: (fun _ -> assert_equal 1 1)

let test_empty = "testing empty net" >:: (fun _ ->
  let p = empty in
  assert_equal (places p) [];
  assert_equal (transitions p) [];
  assert_equal (arcs p) [];
)

let test_add_place = "testing add place" >:: (fun _ ->
  let p = empty |> add_place (mk_place "A") 1 0 in
  assert_equal (places p) [mk_place "A"];
  assert_equal (transitions p) [];
  assert_equal (arcs p) [];
)

let test_add_place2 = "testing add place 2" >:: (fun _ ->
  let p = empty |> add_place (mk_place "A") 1 0 |> add_place (mk_place "B") 1 0 in
  assert_equal (List.length (places p)) 2;
  assert_bool "A should be a place" (List.mem (mk_place "A") (places p));
  assert_bool "B should be a place" (List.mem (mk_place "B") (places p));
  assert_equal (transitions p) []; 
  assert_equal (arcs p) [];
)

let test_add_transition = "testing add transition" >:: (fun _ ->
  let p = empty |> add_transition (mk_transition "T") in
  assert_equal (transitions p) [mk_transition "T"];
  assert_equal (places p) [];
  assert_equal (arcs p) [];
)

let test_add_arc = "testing add arc" >:: (fun _ ->
  let p = empty 
  |> add_place (mk_place "A") 1 0 
  |> add_place (mk_place "B") 1 0 
  |> add_transition (mk_transition "T") 
  |> add_arc (mk_outgoing_arc (mk_place "A") (mk_transition "T") 1) 
  |> function 
    | None -> assert_failure "Failed to add arc"
    | Some net -> net
  in
  assert_equal (arcs p) [(mk_outgoing_arc (mk_place "A") (mk_transition "T") 1)];
) 

let test_add_arc2 = "testing add arc" >:: (fun _ ->
  let _ = empty 
  |> add_place (mk_place "A") 1 0 
  |> add_place (mk_place "B") 1 0 
  |> add_transition (mk_transition "T") 
  |> add_arc (mk_outgoing_arc (mk_place "C") (mk_transition "T") 1) 
  |> function 
    | None -> ()
    | Some _ -> assert_failure "Should not add arc"
  in ()
) 

let is_enabled_test = "testing is enabled" >:: (fun _ ->
  let p = 
    empty 
    |> add_place (mk_place "A") 1 0 
    |> add_transition (mk_transition "T") 
    |> add_arc (mk_outgoing_arc (mk_place "A") (mk_transition "T") 1)
  in match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p -> assert_bool "Should be disabled" (not (is_enabled (mk_transition "T") p));
)
let is_enabled_test2 = "testing is enabled 2" >:: (fun _ ->
  let p = 
    empty 
    |> add_place (mk_place "A") 1 0 
    |> add_transition (mk_transition "T") 
    |> add_arc (mk_outgoing_arc (mk_place "A") (mk_transition "T") 1)
  in match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p -> assert_bool "Should not be enabled" (not (is_enabled (mk_transition "T") p))
)

let tests = [
  canary_test
  ; test_empty
  ; test_add_place
  ; test_add_place2
  ; test_add_transition
  ; test_add_arc
  ; test_add_arc2
  ; is_enabled_test
  ; is_enabled_test2
  (*; test_marking_of_place *)
  ]

let test_suite_unit_tests = "Unit tests" >::: tests;;