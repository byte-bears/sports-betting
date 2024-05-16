(* open OUnit2 open Sports_betting

   let test_columns = "test suite for column operations" >::: [ ( "empty" >::
   fun _ -> assert_equal (Column.empty 2) { data = Array.make 2 ""; size = 0 }
   ); ("append" >:: let col = Column.empty 2 in Column.add col "1"; fun _ ->
   assert_equal Column.{ data = [| "1"; "" |]; size = 1 } col); ("append2" >::
   let col = Column.empty 5 in Column.add col "1"; Column.add col "3";
   Column.add col "2"; Column.add col "1"; fun _ -> assert_equal Column.{ data =
   [| "1"; "3"; "2"; "1"; "" |]; size = 4 } col); ("remove" >:: let col =
   Column.empty 6 in Column.add col "1"; Column.add col "3"; Column.add col "2";
   Column.add col "1"; Column.add col "5"; Column.remove col; Column.remove col;
   fun _ -> assert_equal Column.{ data = [| "1"; "3"; "2"; ""; ""; "" |]; size =
   3 } col); ( "remove2" >:: fun _ -> let col = Column.empty 2 in Column.add col
   "1"; Column.remove col; assert_equal Column.{ data = [| ""; "" |]; size = 0 }
   col ); ( "get" >:: fun _ -> let col = Column.empty 2 in Column.add col "1";
   Column.add col "5"; assert_equal (Column.get col 1) "5" ); ( "get2" >:: fun _
   -> let col = Column.empty 6 in Column.add col "1"; Column.add col "3";
   Column.add col "2"; Column.add col "1"; Column.add col "5"; assert_equal
   (Column.get col 3) "1" ); ( "make" >:: fun _ -> let col = Column.make [ "1";
   "5" ] (Column.empty 2) in assert_equal Column.{ data = [| "1"; "5" |]; size =
   2 } col ); ( "fold left" >:: fun _ -> let temp = Column.make [ "1"; "5" ]
   (Column.empty 2) in let col = Column.fold_left (fun x y -> string_of_int
   (int_of_string x + int_of_string y)) "0" temp in assert_equal "6" col ); (
   "fold left2" >:: fun _ -> let temp = Column.make [ "C"; "S"; "3"; "1"; "1";
   "0" ] (Column.empty 6) in let col = Column.fold_left (fun x y -> x ^ y) "0"
   temp in assert_equal "CS3110" col ); ( "sub" >:: fun _ -> let temp =
   Column.make [ "1"; "3"; "2"; "4"; "7"; "6" ] (Column.empty 6) in let col =
   Column.sub temp 0 4 in assert_equal Column.{ data = [| "1"; "3"; "2"; "4";
   ""; "" |]; size = 4 } col ); ( "sub2" >:: fun _ -> let temp = Column.make [
   "1"; "3"; "2"; "4" ] (Column.empty 4) in let col = Column.sub temp 0 2 in
   assert_equal Column.{ data = [| "1"; "3"; ""; "" |]; size = 2 } col ); ]

   let rec create_test_table columns = match columns with | [] ->
   Datatable.empty | h :: t -> A3.Datatable.append (create_test_table t) h

   let test_datatable = "test suite for datatable operations" >::: [
   ("load_csv_empty" >:: fun _ -> assert_equal [] (load_csv empty_test)); (
   "load_csv" >:: fun _ -> let test_load = load_csv test in let columns = [ [
   "LatD"; "41"; "42"; "46" ]; [ "LatM"; "5"; "52"; "35" ]; [ "LatS"; "59";
   "48"; "59" ]; ] in assert_equal (create_test_table columns) test_load ); (
   "append" >:: fun _ -> let test_append = A3.Datatable.append (load_csv test) [
   "LatE"; "10"; "5"; "10" ] in let columns = [ [ "LatE"; "10"; "5"; "10" ]; [
   "LatD"; "41"; "42"; "46" ]; [ "LatM"; "5"; "52"; "35" ]; [ "LatS"; "59";
   "48"; "59" ]; ] in assert_equal (create_test_table columns) test_append ); (
   "remove" >:: fun _ -> let test_append = A3.Datatable.remove (load_csv test)
   "LatD" in let columns = [ [ "LatM"; "5"; "52"; "35" ]; [ "LatS"; "59"; "48";
   "59" ] ] in assert_equal (create_test_table columns) test_append ); ( "map"
   >:: fun _ -> let test_map = A3.Datatable.map (fun x -> string_of_int
   (int_of_string x + 5)) (load_csv test) "LatD" in let columns = [ [ "LatD";
   "46"; "47"; "51" ]; [ "LatM"; "5"; "52"; "35" ]; [ "LatS"; "59"; "48"; "59"
   ]; ] in assert_equal (create_test_table columns) test_map ); ( "filter" >::
   fun _ -> let test_filter = A3.Datatable.filter (fun x -> int_of_string x < 6)
   (load_csv test) "LatM" in let columns = [ [ "LatD"; "41" ]; [ "LatM"; "5" ];
   [ "LatS"; "59" ] ] in assert_equal (create_test_table columns) test_filter );
   ( "reduce" >:: fun _ -> let test_reduce = A3.Datatable.reduce (fun x y ->
   string_of_int (int_of_string x + int_of_string y)) (load_csv test) "0" "LatM"
   in assert_equal "92" test_reduce ); ]

   let list_simple_average_tests = "simple moving average functions tests for\n
   lists" >::: [ ( "empty list test" >:: fun _ -> assert_equal 0.
   (Ema.simple_average_list []) ); ( "increasing list 1" >:: fun _ ->
   assert_equal 2.5 (Ema.simple_average_list [ 1.; 2.; 3.; 4. ]) ); (
   "decreasing list 1" >:: fun _ -> assert_equal 2.5 (Ema.simple_average_list
   (List.rev [ 1.; 2.; 3.; 4. ])) ); ( "constant list 1" >:: fun _ ->
   assert_equal 1. (Ema.simple_average_list [ 1.; 1.; 1.; 1. ]) ); (
   "non-decreasing list 1" >:: fun _ -> assert_equal 2.5
   (Ema.simple_average_list [ 2.; 2.; 3.; 3. ]) ); ( "non-increasing list 1" >::
   fun _ -> assert_equal 3.5 (Ema.simple_average_list [ 4.; 4.; 3.; 3. ]) ); (
   "increasing list 2" >:: fun _ -> assert_equal 3.5 (Ema.simple_average_list [
   2.; 3.; 4.; 5. ]) ); ( "decreasing list 2" >:: fun _ -> assert_equal 3.5
   (Ema.simple_average_list (List.rev [ 2.; 3.; 4.; 5. ])) ); ( "constant list\n
   2" >:: fun _ -> assert_equal 2. (Ema.simple_average_list [ 2.; 2.; 2.; 2. ])
   ); ( "non-decreasing list 2" >:: fun _ -> assert_equal 3.5
   (Ema.simple_average_list [ 3.; 3.; 4.; 4. ]) ); ( "non-increasing list 2" >::
   fun _ -> assert_equal 3. (Ema.simple_average_list [ 4.; 4.; 2.; 2. ]) ); ]

   let list_sma_tests = "simple moving average functions tests for lists" >::: [
   ( "empty list" >:: fun _ -> assert_equal [] (Ema.simple_moving_average_list
   []) ); ( "increasing list 1" >:: fun _ -> assert_equal [ 1.; 1.5; 2.; 2.5 ]
   (Ema.simple_moving_average_list [ 1.; 2.; 3.; 4. ]) ); ( "decreasing list 1"
   >:: fun _ -> assert_equal [ 4.; 3.5; 3.; 2.5 ]
   (Ema.simple_moving_average_list (List.rev [ 1.; 2.; 3.; 4. ])) ); (
   "constant\n list 1" >:: fun _ -> assert_equal [ 1.; 1.; 1.; 1. ]
   (Ema.simple_moving_average_list [ 1.; 1.; 1.; 1. ]) ); ( "increasing list 2"
   >:: fun _ -> assert_equal [ 2.; 3.; 4.; 5. ] (Ema.simple_moving_average_list
   [ 2.; 4.; 6.; 8. ]) ); ( "decreasing list 2" >:: fun _ -> assert_equal [ 8.;
   7.; 6.; 5. ] (Ema.simple_moving_average_list (List.rev [ 2.; 4.; 6.; 8. ]))
   ); ( "constant list 2" >:: fun _ -> assert_equal [ 2.; 2.; 2.; 2. ]
   (Ema.simple_moving_average_list [ 2.; 2.; 2.; 2. ]) ); ]

   let list_wma_tests = "weighted moving average functions tests for lists" >:::
   [ ( "empty list for epsilon less than 0.5" >:: fun _ -> assert_equal []
   (Ema.weighted_moving_average_list 0.4 []) ); ( "1 increasing list for
   epsilon\n less than 0.5" >:: fun _ -> assert_equal [ 1.; 1.6; 2.44; 3.376 ]
   (Ema.weighted_moving_average_list 0.4 [ 1.; 2.; 3.; 4. ]) ); ( "1
   decreasing\n list for epsilon less than 0.5" >:: fun _ -> assert_equal [ 4.;
   3.4; 2.56; 1.624 ] (Ema.weighted_moving_average_list 0.4 (List.rev [ 1.; 2.;
   3.; 4. ])) ); ( "1 constant list for epsilon greater than 0.5" >:: fun _ ->
   assert_equal [ 1.; 1.; 1.; 1. ] (Ema.weighted_moving_average_list 0.4 [ 1.;
   1.; 1.; 1. ]) ); ( "1 empty list for epsilon greater than 0.5" >:: fun _ ->
   assert_equal [] (Ema.weighted_moving_average_list 0.6 []) ); ( "1 increasing
   list for epsilon\n greater than 0.5" >:: fun _ -> assert_equal [ 1.; 1.4;
   2.04; 2.824 ] (Ema.weighted_moving_average_list 0.6 [ 1.; 2.; 3.; 4. ]) ); (
   "1 decreasing\n list for epsilon greater than 0.5" >:: fun _ -> assert_equal
   [ 4.; 3.6; 2.96; 2.176 ] (Ema.weighted_moving_average_list 0.6 (List.rev [
   1.; 2.; 3.; 4. ])) ); ( "1 constant list for epsilon greater than 0.5" >::
   fun _ -> assert_equal [ 1.; 1.; 1.; 1. ] (Ema.weighted_moving_average_list
   0.6 [ 1.; 1.; 1.; 1. ]) ); ( "2 increasing list for epsilon less than 0.5"
   >:: fun _ -> assert_equal [ 2.; 3.2; 4.88; 6.752 ]
   (Ema.weighted_moving_average_list 0.4 [ 2.; 4.; 6.; 8. ]) ); ( "2 decreasing
   list for epsilon less than 0.5" >:: fun _ -> assert_equal [ 8.; 6.8; 5.12;
   3.248 ] (Ema.weighted_moving_average_list 0.4 (List.rev [ 2.; 4.; 6.; 8. ]))
   ); ( "2 constant list for epsilon greater than\n 0.5" >:: fun _ ->
   assert_equal [ 2.; 2.; 2.; 2. ] (Ema.weighted_moving_average_list 0.4 [ 2.;
   2.; 2.; 2. ]) ); ( "2 increasing\n list for epsilon greater than 0.5" >:: fun
   _ -> assert_equal [ 2.; 2.8; 4.08; 5.648 ] (Ema.weighted_moving_average_list
   0.6 [ 2.; 4.; 6.; 8. ]) ); ( "2\n decreasing list for epsilon greater than
   0.5" >:: fun _ -> assert_equal [ 8.; 7.2; 5.92; 4.352 ]
   (Ema.weighted_moving_average_list 0.6 (List.rev [ 2.; 4.; 6.; 8. ])) ); ( "2
   constant list for epsilon greater than 0.5" >:: fun _ -> assert_equal [ 2.;
   2.; 2.; 2. ] (Ema.weighted_moving_average_list 0.6 [ 2.; 2.; 2.; 2. ]) ); ]

   let _ = run_test_tt_main test_datatable let _ = run_test_tt_main
   list_wma_tests let _ = run_test_tt_main list_sma_tests let _ =
   run_test_tt_main list_simple_average_tests *)
