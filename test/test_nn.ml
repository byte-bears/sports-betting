open OUnit2
open Sports_betting
open Owl

let test_initialize_weights_valid_dimensions _ =
  let weights = Neural_network.initialize_weights 10 5 in
  assert_equal (Mat.row_num weights) 10;
  assert_equal (Mat.col_num weights) 5

let test_initialize_weights_check_scale _ =
  let input_dim = 10 in
  let output_dim = 5 in
  let scale = sqrt (2. /. float_of_int input_dim) in
  let weights = Neural_network.initialize_weights input_dim output_dim in
  let stddev = Mat.std weights in
  assert_bool "Standard deviation is approximately correct"
    (abs_float (stddev -. scale) < 0.1)

let test_initialize_weights_negative_dims _ =
  let test_fun () = ignore (Neural_network.initialize_weights (-1) 5) in
  assert_raises (Invalid_argument "Negative dimensions not allowed") test_fun

let test_initialize_biases_valid_dimensions _ =
  let biases = Neural_network.initialize_biases 5 in
  assert_equal (Mat.row_num biases) 1;
  assert_equal (Mat.col_num biases) 5

let test_initialize_biases_zeros _ =
  let biases = Neural_network.initialize_biases 5 in
  assert_bool "All biases should be zero" (Mat.sum' biases = 0.)

let test_initialize_biases_negative_dims _ =
  let test_fun () = ignore (Neural_network.initialize_biases (-5)) in
  assert_raises (Invalid_argument "Negative dimensions not allowed") test_fun

let suite =
  "Testing NN"
  >::: [
         "test_initialize_weights_valid_dimensions"
         >:: test_initialize_weights_valid_dimensions;
         "test_initialize_weights_check_scale"
         >:: test_initialize_weights_check_scale;
         "test_initialize_weights_negative_dims"
         >:: test_initialize_weights_negative_dims;
         "test_initialize_biases_valid_dimensions"
         >:: test_initialize_biases_valid_dimensions;
         "test_initialize_biases_zeros" >:: test_initialize_biases_zeros;
         "test_initialize_biases_negative_dims"
         >:: test_initialize_biases_negative_dims;
       ]

let () = run_test_tt_main suite
