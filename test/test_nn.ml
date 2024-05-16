(*open OUnit2 open Sports_betting open Owl

  let test_initialize_weights_valid_dimensions _ = let weights =
  Neural_network.initialize_weights 10 5 in assert_equal (Mat.row_num weights)
  10; assert_equal (Mat.col_num weights) 5

  (* let test_initialize_weight_check_scale _ = let input_dim = 10 in let
  output_dim = 5 in let scale = sqrt (2. /. float_of_int intput_dim) in let
  weights = Neural_network.initialize_weights input_dim output_dim in let
  flat_weights = Mat.reshape weights [| 1; input_dim * output_dim |] in (*
  Flatten the matrix *) let stddev = Mat.std flat_weights in let epsilon = 0.1
  in let msg = Printf.sprintf "Expected scale: %.3f, observed stddev: %.3f"
  scale (Mat.print stddev) in assert_bool msg (abs_float (stddev -. scale) <
  epsilon) *)

  let test_initialize_weights_negative_dims _ = let test_fun () = ignore
  (Neural_network.initialize_weights (-1) 5) in assert_raises (Invalid_argument
  "Negative dimensions not allowed") test_fun

  let suite = "Testing NN" >::: [ "test_initialize_weights_valid_dimensions" >::
  test_initialize_weights_valid_dimensions; (*
  "test_initialize_weights_check_scale" >:: test_initialize_weights_check_scale;
  *) "test_initialize_weights_negative_dims" >::
  test_initialize_weights_negative_dims; ]

  let () = run_test_tt_main suite *)
