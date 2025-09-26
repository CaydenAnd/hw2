open OUnit2
open Basics

let test_sanity _ = assert_equal 1 1

let test_rev_triple _ = assert_equal ('c','b','a') (rev_triple ('a','b','c'))

let test_is_odd_negative _ =
  assert_equal true (is_odd (-3));
  assert_equal false (is_odd (-4))

let test_is_older_equal _ =
  assert_bool "" (not (is_older (2025,9,25) (2025,9,25)))

let test_pow_zero _ = assert_equal 1 (pow 7 0)

let test_get_nth_oob _ =
  assert_raises (Failure "get_nth: index out of bounds")
    (fun () -> ignore (get_nth (5,[1;2])))

let suite =
  "student" >::: [
    "sanity" >:: test_sanity;
    "rev_triple" >:: test_rev_triple;
    "is_odd_negative" >:: test_is_odd_negative;
    "is_older_equal" >:: test_is_older_equal;
    "pow_zero" >:: test_pow_zero;
    "get_nth_oob" >:: test_get_nth_oob;
  ]

let () = run_test_tt_main suite
