open OUnit2
open Sports_betting

let test_columns =
  "test suite for column operations"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal (Column.empty 2) { data = Array.make 2 ""; size = 0 } );
         ( "empty2" >:: fun _ ->
           assert_equal (Column.empty 7) { data = Array.make 7 ""; size = 0 } );
         ( "empty3" >:: fun _ ->
           assert_equal (Column.empty 13) { data = Array.make 13 ""; size = 0 }
         );
         ("append"
         >::
         let col = Column.empty 2 in
         Column.add col "1";
         fun _ -> assert_equal Column.{ data = [| "1"; "" |]; size = 1 } col);
         ("append2"
         >::
         let col = Column.empty 5 in
         Column.add col "1";
         Column.add col "3";
         Column.add col "2";
         Column.add col "1";
         fun _ ->
           assert_equal
             Column.{ data = [| "1"; "3"; "2"; "1"; "" |]; size = 4 }
             col);
         ("append3"
         >::
         let col = Column.empty 8 in
         Column.add col "1";
         Column.add col "-1";
         Column.add col "8";
         Column.add col "-100";
         Column.add col "390";
         Column.add col "0";
         fun _ ->
           assert_equal
             Column.
               {
                 data = [| "1"; "-1"; "8"; "-100"; "390"; "0"; ""; "" |];
                 size = 6;
               }
             col);
         ("remove"
         >::
         let col = Column.empty 6 in
         Column.add col "1";
         Column.add col "3";
         Column.add col "2";
         Column.add col "1";
         Column.add col "5";
         Column.remove col;
         Column.remove col;
         fun _ ->
           assert_equal
             Column.{ data = [| "1"; "3"; "2"; ""; ""; "" |]; size = 3 }
             col);
         ( "remove2" >:: fun _ ->
           let col = Column.empty 2 in
           Column.add col "1";
           Column.remove col;
           assert_equal Column.{ data = [| ""; "" |]; size = 0 } col );
         ( "remove3 - raise error" >:: fun _ ->
           let col = Column.empty 1 in
           Column.add col "1";
           Column.remove col;
           assert_raises Column.Out_of_bounds (fun () -> Column.remove col) );
         ( "remove4 - raises error" >:: fun _ ->
           let col = Column.empty 6 in
           Column.add col "1";
           Column.add col "3";
           Column.add col "2";
           Column.add col "1";
           Column.add col "5";
           Column.remove col;
           Column.remove col;
           Column.remove col;
           Column.remove col;
           Column.remove col;
           assert_raises Column.Out_of_bounds (fun () -> Column.remove col) );
         ("capacity"
         >::
         let col = Column.empty 3 in
         fun _ -> assert_equal 3 (Column.capacity col));
         ("capacity2"
         >::
         let col = Column.empty 6 in
         fun _ -> assert_equal 6 (Column.capacity col));
         ("capacity3"
         >::
         let col = Column.empty 1 in
         fun _ -> assert_equal 1 (Column.capacity col));
         ( "get" >:: fun _ ->
           let col = Column.empty 2 in
           Column.add col "1";
           Column.add col "5";
           assert_equal (Column.get col 1) "5" );
         ( "get2" >:: fun _ ->
           let col = Column.empty 6 in
           Column.add col "1";
           Column.add col "3";
           Column.add col "2";
           Column.add col "1";
           Column.add col "5";
           assert_equal (Column.get col 3) "1" );
         ( "get3 - raise error" >:: fun _ ->
           let col = Column.empty 6 in
           Column.add col "1";
           Column.add col "3";
           Column.add col "2";
           Column.add col "1";
           Column.add col "5";
           assert_raises Column.Out_of_bounds (fun () -> Column.get col 19) );
         ( "get4 - raise error" >:: fun _ ->
           let col = Column.empty 6 in
           assert_raises Column.Out_of_bounds (fun () -> Column.get col 1) );
         ( "make" >:: fun _ ->
           let col = Column.make [ "1"; "5" ] (Column.empty 2) in
           assert_equal Column.{ data = [| "1"; "5" |]; size = 2 } col );
         ( "fold left" >:: fun _ ->
           let temp = Column.make [ "1"; "5" ] (Column.empty 2) in
           let col =
             Column.fold_left
               (fun x y -> string_of_int (int_of_string x + int_of_string y))
               "0" temp
           in
           assert_equal "6" col );
         ( "fold left2" >:: fun _ ->
           let temp =
             Column.make [ "C"; "S"; "3"; "1"; "1"; "0" ] (Column.empty 6)
           in
           let col = Column.fold_left (fun x y -> x ^ y) "" temp in
           assert_equal "CS3110" col );
         ( "sub" >:: fun _ ->
           let temp =
             Column.make [ "1"; "3"; "2"; "4"; "7"; "6" ] (Column.empty 6)
           in
           let col = Column.sub temp 0 4 in
           assert_equal Column.{ data = [| "1"; "3"; "2"; "4" |]; size = 4 } col
         );
         ( "sub2" >:: fun _ ->
           let temp = Column.make [ "1"; "3"; "2"; "4" ] (Column.empty 4) in
           let col = Column.sub temp 0 2 in
           assert_equal Column.{ data = [| "1"; "3" |]; size = 2 } col );
         ( "sub3" >:: fun _ ->
           let temp = Column.make [ "1"; "3"; "2"; "4" ] (Column.empty 4) in
           let col = Column.sub temp 0 0 in
           assert_equal Column.{ data = [||]; size = 0 } col );
         ( "extend" >:: fun _ ->
           let temp = Column.make [ "1"; "3"; "2"; "4" ] (Column.empty 4) in
           let col = Column.extend temp 6 in
           assert_equal
             Column.{ data = [| "1"; "3"; "2"; "4"; ""; "" |]; size = 4 }
             col );
         ( "extend2" >:: fun _ ->
           let temp = Column.make [ "1"; "3"; "2"; "4" ] (Column.empty 4) in
           let col = Column.extend temp 0 in
           assert_equal Column.{ data = [| "1"; "3"; "2"; "4" |]; size = 4 } col
         );
         ( "extend3" >:: fun _ ->
           let temp = Column.make [] (Column.empty 0) in
           let col = Column.extend temp 4 in
           assert_equal Column.{ data = [| ""; ""; ""; "" |]; size = 0 } col );
         ( "to_string1" >:: fun _ ->
           let col = Column.empty 5 in
           Column.add col "a";
           Column.add col "bb";
           Column.add col "ccc";
           Column.add col "dddd";
           Column.add col "eeeee";
           assert_equal "a    |bb   |ccc  |dddd |eeeee" (Column.to_string 5 col)
         );
         ( "to_string2" >:: fun _ ->
           let col = Column.empty 6 in
           Column.add col "cs1110";
           Column.add col "cs2110";
           Column.add col "cs2800";
           Column.add col "cs3110";
           Column.add col "cs3410";
           Column.add col "cs4820";
           assert_equal "cs1110 |cs2110 |cs2800 |cs3110 |cs3410 |cs4820"
             (Column.to_string 7 col) );
         ( "to_string3 - raise error" >:: fun _ ->
           let col = Column.empty 6 in
           Column.add col "EA sports";
           Column.add col "reeeeee";
           Column.add col "dab";
           Column.add col "a";

           assert_raises Column.Out_of_bounds (fun () -> Column.to_string 0 col)
         );
         ( "max_length1" >:: fun _ ->
           let col = Column.empty 5 in
           Column.add col "a";
           Column.add col "bb";
           Column.add col "ccc";
           Column.add col "dddd";
           Column.add col "eeeee";
           assert_equal 5 (Column.max_length col) );
         ( "max_length2" >:: fun _ ->
           let col = Column.empty 6 in
           Column.add col "";
           Column.add col "ECE 2300 is a great class";
           Column.add col "I love cheeseburgers";
           Column.add col "Free Merch!!!!";
           Column.add col "Wahooooo, my name is Mario";
           Column.add col "I guarantee I am not on mushrooms";
           assert_equal 33 (Column.max_length col) );
         ( "max_length3 - raise error" >:: fun _ ->
           let col = Column.empty 6 in
           assert_equal 0 (Column.max_length col) );
       ]

let test_datatable =
  "test suite for datatable operations"
  >::: [
         ( "empty 1x1 datatable" >:: fun _ ->
           let empty_table = Datatable.empty 1 1 in
           assert_equal
             Datatable.
               {
                 headers = [| " " |];
                 dt = [| Column.{ data = [| "" |]; size = 0 } |];
                 size = 0;
               }
             empty_table );
         ( "empty 3x1 datatable" >:: fun _ ->
           let empty_table = Datatable.empty 3 1 in
           assert_equal
             Datatable.
               {
                 headers = [| " "; " "; " " |];
                 dt =
                   [|
                     Column.{ data = [| "" |]; size = 0 };
                     Column.{ data = [| "" |]; size = 0 };
                     Column.{ data = [| "" |]; size = 0 };
                   |];
                 size = 0;
               }
             empty_table );
         ( "add column" >:: fun _ ->
           let table = Datatable.empty 3 1 in
           let col = Column.empty 1 in
           Column.add col "4";
           Datatable.add table "test" col;
           assert_equal
             Datatable.
               {
                 headers = [| "test"; " "; " " |];
                 dt =
                   [|
                     Column.{ data = [| "4" |]; size = 1 };
                     Column.{ data = [| "" |]; size = 0 };
                     Column.{ data = [| "" |]; size = 0 };
                   |];
                 size = 1;
               }
             table );
         ( "add two columns" >:: fun _ ->
           let table = Datatable.empty 3 1 in
           let col1 = Column.empty 1 in
           let col2 = Column.empty 1 in
           Column.add col1 "4";
           Column.add col2 "5";
           Datatable.add table "test1" col1;
           Datatable.add table "test2" col2;
           assert_equal
             Datatable.
               {
                 headers = [| "test1"; "test2"; " " |];
                 dt =
                   [|
                     Column.{ data = [| "4" |]; size = 1 };
                     Column.{ data = [| "5" |]; size = 1 };
                     Column.{ data = [| "" |]; size = 0 };
                   |];
                 size = 2;
               }
             table );
         ( "add column - column expansion " >:: fun _ ->
           let table = Datatable.empty 3 3 in
           let col = Column.empty 1 in
           Column.add col "4";
           Datatable.add table "test" col;
           assert_equal
             Datatable.
               {
                 headers = [| "test"; " "; " " |];
                 dt =
                   [|
                     Column.{ data = [| "4"; ""; "" |]; size = 1 };
                     Column.{ data = [| ""; ""; "" |]; size = 0 };
                     Column.{ data = [| ""; ""; "" |]; size = 0 };
                   |];
                 size = 1;
               }
             table );
         ( "add column - column compression " >:: fun _ ->
           let table = Datatable.empty 3 3 in
           let col = Column.empty 6 in
           Column.add col "1";
           Column.add col "2";
           Column.add col "3";
           Column.add col "4";
           Column.add col "5";
           Column.add col "6";
           Datatable.add table "test" col;
           assert_equal
             Datatable.
               {
                 headers = [| "test"; " "; " " |];
                 dt =
                   [|
                     Column.{ data = [| "1"; "2"; "3" |]; size = 3 };
                     Column.{ data = [| ""; ""; "" |]; size = 0 };
                     Column.{ data = [| ""; ""; "" |]; size = 0 };
                   |];
                 size = 1;
               }
             table );
         ( "max_length simple" >:: fun _ ->
           assert_equal 1
             (Datatable.max_length
                Datatable.
                  {
                    headers = [| "test1"; "test2"; " " |];
                    dt =
                      [|
                        Column.{ data = [| "4" |]; size = 1 };
                        Column.{ data = [| "5" |]; size = 1 };
                        Column.{ data = [| "" |]; size = 0 };
                      |];
                    size = 3;
                  }) );
         ( "max_length complex" >:: fun _ ->
           assert_equal 11
             (Datatable.max_length
                Datatable.
                  {
                    headers = [| "test1"; "test2"; " " |];
                    dt =
                      [|
                        Column.{ data = [| "4"; "5123"; "9" |]; size = 3 };
                        Column.
                          { data = [| "5"; "hello"; "hello world" |]; size = 3 };
                        Column.{ data = [| "" |]; size = 0 };
                      |];
                    size = 3;
                  }) );
         ( "max_length empty" >:: fun _ ->
           assert_equal 0
             (Datatable.max_length
                Datatable.
                  {
                    headers = [| "test1"; "test2"; " " |];
                    dt =
                      [|
                        Column.{ data = [||]; size = 0 };
                        Column.{ data = [||]; size = 0 };
                        Column.{ data = [||]; size = 0 };
                      |];
                    size = 3;
                  }) );
         ( "to string empty" >:: fun _ ->
           assert_equal "\n ||\n ||\n ||"
             (Datatable.to_string
                Datatable.
                  {
                    headers = [| ""; ""; "" |];
                    dt =
                      [|
                        Column.{ data = [||]; size = 0 };
                        Column.{ data = [||]; size = 0 };
                        Column.{ data = [||]; size = 0 };
                      |];
                    size = 3;
                  }) );
         ( "to string 1 column" >:: fun _ ->
           assert_equal "\ntest1 ||1\n2     ||2\n3     ||3"
             (Datatable.to_string
                Datatable.
                  {
                    headers = [| "test1"; "2"; "3" |];
                    dt =
                      [|
                        Column.{ data = [| "1" |]; size = 0 };
                        Column.{ data = [| "2" |]; size = 0 };
                        Column.{ data = [| "3" |]; size = 0 };
                      |];
                    size = 3;
                  }) );
         ( "to string 2 column" >:: fun _ ->
           assert_equal "\ntest1 ||1     |2\nhi    ||2     |3\nhello ||3     |4"
             (Datatable.to_string
                Datatable.
                  {
                    headers = [| "test1"; "hi"; "hello" |];
                    dt =
                      [|
                        Column.{ data = [| "1"; "2" |]; size = 0 };
                        Column.{ data = [| "2"; "3" |]; size = 0 };
                        Column.{ data = [| "3"; "4" |]; size = 0 };
                      |];
                    size = 3;
                  }) );
         ( "to string 3 column" >:: fun _ ->
           assert_equal
             "\n\
              test1       ||1           |2           |3\n\
              hi          ||2           |3           |hello world\n\
              asdf        ||3           |4           |hi"
             (Datatable.to_string
                Datatable.
                  {
                    headers = [| "test1"; "hi"; "asdf" |];
                    dt =
                      [|
                        Column.{ data = [| "1"; "2"; "3" |]; size = 0 };
                        Column.
                          { data = [| "2"; "3"; "hello world" |]; size = 0 };
                        Column.{ data = [| "3"; "4"; "hi" |]; size = 0 };
                      |];
                    size = 3;
                  }) );
         ( "to string 3 column with column name as longest" >:: fun _ ->
           assert_equal
             "\n\
              test1sdf ||1        |2        |3\n\
              hi       ||2        |3        |hello\n\
              asdf     ||3        |4        |hi"
             (Datatable.to_string
                Datatable.
                  {
                    headers = [| "test1sdf"; "hi"; "asdf" |];
                    dt =
                      [|
                        Column.{ data = [| "1"; "2"; "3" |]; size = 0 };
                        Column.{ data = [| "2"; "3"; "hello" |]; size = 0 };
                        Column.{ data = [| "3"; "4"; "hi" |]; size = 0 };
                      |];
                    size = 3;
                  }) );
       ]

let list_simple_average_tests =
  "simple moving average functions tests for\n\n   lists"
  >::: [
         ( "empty list test" >:: fun _ ->
           assert_equal 0. (Ema.simple_average_list []) );
         ( "increasing list 1" >:: fun _ ->
           assert_equal 2.5 (Ema.simple_average_list [ 1; 2; 3; 4 ]) );
         ( "decreasing list 1" >:: fun _ ->
           assert_equal 2.5 (Ema.simple_average_list (List.rev [ 1; 2; 3; 4 ]))
         );
         ( "constant list 1" >:: fun _ ->
           assert_equal 1. (Ema.simple_average_list [ 1; 1; 1; 1 ]) );
         ( "non-decreasing list 1" >:: fun _ ->
           assert_equal 2.5 (Ema.simple_average_list [ 2; 2; 3; 3 ]) );
         ( "non-increasing list 1" >:: fun _ ->
           assert_equal 3.5 (Ema.simple_average_list [ 4; 4; 3; 3 ]) );
         ( "increasing list 2" >:: fun _ ->
           assert_equal 3.5 (Ema.simple_average_list [ 2; 3; 4; 5 ]) );
         ( "decreasing list 2" >:: fun _ ->
           assert_equal 3.5 (Ema.simple_average_list (List.rev [ 2; 3; 4; 5 ]))
         );
         ( "constant list\n\n   2" >:: fun _ ->
           assert_equal 2. (Ema.simple_average_list [ 2; 2; 2; 2 ]) );
         ( "non-decreasing list 2" >:: fun _ ->
           assert_equal 3.5 (Ema.simple_average_list [ 3; 3; 4; 4 ]) );
         ( "non-increasing list 2" >:: fun _ ->
           assert_equal 3. (Ema.simple_average_list [ 4; 4; 2; 2 ]) );
       ]

let list_sma_tests =
  "simple moving average functions tests for lists"
  >::: [
         ( "empty list" >:: fun _ ->
           assert_equal [] (Ema.simple_moving_average_list []) );
         ( "increasing list 1" >:: fun _ ->
           assert_equal [ 1.; 1.5; 2.; 2.5 ]
             (Ema.simple_moving_average_list [ 1.; 2.; 3.; 4. ]) );
         ( "decreasing list 1" >:: fun _ ->
           assert_equal [ 4.; 3.5; 3.; 2.5 ]
             (Ema.simple_moving_average_list (List.rev [ 1.; 2.; 3.; 4. ])) );
         ( "constant\n list 1" >:: fun _ ->
           assert_equal [ 1.; 1.; 1.; 1. ]
             (Ema.simple_moving_average_list [ 1.; 1.; 1.; 1. ]) );
         ( "increasing list 2" >:: fun _ ->
           assert_equal [ 2.; 3.; 4.; 5. ]
             (Ema.simple_moving_average_list [ 2.; 4.; 6.; 8. ]) );
         ( "decreasing list 2" >:: fun _ ->
           assert_equal [ 8.; 7.; 6.; 5. ]
             (Ema.simple_moving_average_list (List.rev [ 2.; 4.; 6.; 8. ])) );
         ( "constant list 2" >:: fun _ ->
           assert_equal [ 2.; 2.; 2.; 2. ]
             (Ema.simple_moving_average_list [ 2.; 2.; 2.; 2. ]) );
       ]

let list_wma_tests =
  "weighted moving average functions tests for lists"
  >::: [
         ( "empty list for epsilon less than 0.5" >:: fun _ ->
           assert_equal [] (Ema.weighted_moving_average_list 0.4 []) );
         ( "1 increasing list for\n   epsilon\n less than 0.5" >:: fun _ ->
           assert_equal [ 1.; 1.6; 2.44; 3.376 ]
             (Ema.weighted_moving_average_list 0.4 [ 1.; 2.; 3.; 4. ]) );
         ( "1\n   decreasing\n list for epsilon less than 0.5" >:: fun _ ->
           assert_equal [ 4.; 3.4; 2.56; 1.624 ]
             (Ema.weighted_moving_average_list 0.4
                (List.rev [ 1.; 2.; 3.; 4. ])) );
         ( "1 constant list for epsilon greater than 0.5" >:: fun _ ->
           assert_equal [ 1.; 1.; 1.; 1. ]
             (Ema.weighted_moving_average_list 0.4 [ 1.; 1.; 1.; 1. ]) );
         ( "1 empty list for epsilon greater than 0.5" >:: fun _ ->
           assert_equal [] (Ema.weighted_moving_average_list 0.6 []) );
         ( "1 increasing\n   list for epsilon\n greater than 0.5" >:: fun _ ->
           assert_equal [ 1.; 1.4; 2.04; 2.824 ]
             (Ema.weighted_moving_average_list 0.6 [ 1.; 2.; 3.; 4. ]) );
         ( "1 decreasing\n list for epsilon greater than 0.5" >:: fun _ ->
           assert_equal [ 4.; 3.6; 2.96; 2.176 ]
             (Ema.weighted_moving_average_list 0.6
                (List.rev [ 1.; 2.; 3.; 4. ])) );
         ( "1 constant list for epsilon greater than 0.5" >:: fun _ ->
           assert_equal [ 1.; 1.; 1.; 1. ]
             (Ema.weighted_moving_average_list 0.6 [ 1.; 1.; 1.; 1. ]) );
         ( "2 increasing list for epsilon less than 0.5" >:: fun _ ->
           assert_equal [ 2.; 3.2; 4.88; 6.752 ]
             (Ema.weighted_moving_average_list 0.4 [ 2.; 4.; 6.; 8. ]) );
         ( "2 decreasing\n   list for epsilon less than 0.5" >:: fun _ ->
           assert_equal [ 8.; 6.8; 5.12; 3.248 ]
             (Ema.weighted_moving_average_list 0.4
                (List.rev [ 2.; 4.; 6.; 8. ])) );
         ( "2 constant list for epsilon greater than\n 0.5" >:: fun _ ->
           assert_equal [ 2.; 2.; 2.; 2. ]
             (Ema.weighted_moving_average_list 0.4 [ 2.; 2.; 2.; 2. ]) );
         ( "2 increasing\n list for epsilon greater than 0.5" >:: fun _ ->
           assert_equal [ 2.; 2.8; 4.08; 5.648 ]
             (Ema.weighted_moving_average_list 0.6 [ 2.; 4.; 6.; 8. ]) );
         ( "2\n decreasing list for epsilon greater than\n   0.5" >:: fun _ ->
           assert_equal [ 8.; 7.2; 5.92; 4.352 ]
             (Ema.weighted_moving_average_list 0.6
                (List.rev [ 2.; 4.; 6.; 8. ])) );
         ( "2\n   constant list for epsilon greater than 0.5" >:: fun _ ->
           assert_equal [ 2.; 2.; 2.; 2. ]
             (Ema.weighted_moving_average_list 0.6 [ 2.; 2.; 2.; 2. ]) );
       ]

let _ = run_test_tt_main test_datatable
let _ = run_test_tt_main test_columns
let _ = run_test_tt_main list_wma_tests
let _ = run_test_tt_main list_sma_tests
let _ = run_test_tt_main list_simple_average_tests
