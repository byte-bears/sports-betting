open OUnit2
open Sports_betting

(* Creating some structures to perform testing *)
let mat = Load.load_string_array "test.csv"
let () = Load.make_rectangular_cols mat ""

(* Basic tests *)
let basic_tests =
  "basic test suite for ui helper functions"
  >::: [
         ( "team abbreviations MEM" >:: fun _ ->
           assert_equal (List.mem "MEM" (fst (Processing.teams_list mat))) true
         );
         ( "not team abbreviations" >:: fun _ ->
           assert_equal (List.mem "ASD" (fst (Processing.teams_list mat))) false
         );
         ( "team GSW" >:: fun _ ->
           assert_equal
             (List.mem "Golden State Warriors (GSW)"
                (snd (Processing.teams_list mat)))
             true );
         ( "not team" >:: fun _ ->
           assert_equal (List.mem "" (snd (Processing.teams_list mat))) false );
         ( "curry gsw" >:: fun _ ->
           assert_equal
             (List.mem "Stephen Curry" (Processing.player_list mat "GSW"))
             true );
         ( "lebron not gsw" >:: fun _ ->
           assert_equal
             (List.mem "LeBron James" (Processing.player_list mat "GSW"))
             false );
         ( "lebron lal" >:: fun _ ->
           assert_equal
             (List.mem "LeBron James" (Processing.player_list mat "LAL"))
             true );
       ]

(* Matchups *)
let matchups = Processing.add_matchup_stats mat
let opp = Load.get_col matchups "OPP"
let home = Load.get_col matchups "HOME"

let match_tests =
  "test suite for add matchups"
  >::: [
         ( "opponent" >:: fun _ ->
           assert_equal (Array.sub opp 0 8)
             [| "OPP"; "13."; "10."; "11."; "11."; "11."; "11."; "11." |] );
         ( "home" >:: fun _ ->
           assert_equal (Array.sub home 0 10)
             [| "HOME"; "0."; "1."; "0."; "0."; "0."; "0."; "0."; "0."; "0." |]
         );
         ( "opponent2" >:: fun _ ->
           assert_equal (Array.sub opp 10 8)
             [| "11."; "14."; "11."; "11."; "14."; "14."; "14."; "11." |] );
         ( "home2" >:: fun _ ->
           assert_equal (Array.sub home 10 10)
             [| "0."; "1."; "0."; "0."; "1."; "1."; "1."; "0."; "1."; "0." |] );
       ]

(* Player stat *)
let curry_stats = Processing.get_player_stat mat "Stephen Curry" "PTS"
let wig_stats = Processing.get_player_stat mat "Andrew Wiggins" "REB"
let trae_stats = Processing.get_player_stat mat "Trae Young" "AST"
let lebron_stats = Processing.get_player_stat mat "LeBron James" "BLK"
let gary_stats = Processing.get_player_stat mat "Gary Payton II" "STL"

let player_stat_tests =
  "test suite for player stats"
  >::: [
         ( "curry pts" >:: fun _ ->
           assert_equal
             (Array.sub curry_stats 0 13)
             [| 3.; 47.; 8.; 34.; 15.; 30.; 21.; 34.; 27.; 18.; 25.; 33.; 24. |]
         );
         ( "wig reb" >:: fun _ ->
           assert_equal (Array.sub wig_stats 0 13)
             [| 2.; 4.; 5.; 2.; 8.; 6.; 2.; 4.; 3.; 8.; 8.; 7.; 4. |] );
         ( "trae ast" >:: fun _ ->
           assert_equal
             (Array.sub trae_stats 0 13)
             [| 11.; 8.; 11.; 15.; 10.; 9.; 8.; 16.; 15.; 9.; 8.; 10.; 15. |] );
         ( "lebron blk" >:: fun _ ->
           assert_equal
             (Array.sub lebron_stats 0 13)
             [| 0.; 1.; 1.; 0.; 2.; 2.; 2.; 1.; 1.; 4.; 1.; 1.; 0. |] );
         ( "gary stl" >:: fun _ ->
           assert_equal
             (Array.sub gary_stats 0 13)
             [| 0.; 1.; 2.; 3.; 1.; 1.; 0.; 1.; 2.; 1.; 0.; 1.; 0. |] );
       ]

(* Period data *)
let p_curry =
  Processing.period_data Processing.get_player_stat mat "Stephen Curry" "PTS" 1

let p_wig =
  Processing.period_data Processing.get_player_stat mat "Andrew Wiggins" "REB" 2

let p_trae =
  Processing.period_data Processing.get_player_stat mat "Trae Young" "AST" 3

let p_lebron =
  Processing.period_data Processing.get_player_stat mat "LeBron James" "BLK" 4

let p_gary =
  Processing.period_data Processing.get_player_stat mat "Gary Payton II" "STL" 5

let period_data_tests =
  "test suite for period data generation"
  >::: [
         ( "curry pts mat" >:: fun _ ->
           assert_equal
             (Array.sub (fst p_curry) 0 3)
             [| [| 3. |]; [| 8. |]; [| 15. |] |] );
         ( "wig reb mat" >:: fun _ ->
           assert_equal
             (Array.sub (fst p_wig) 0 3)
             [| [| 2.; 4. |]; [| 2.; 8. |]; [| 2.; 4. |] |] );
         ( "trae ast mat" >:: fun _ ->
           assert_equal
             (Array.sub (fst p_trae) 0 3)
             [| [| 11.; 8.; 11. |]; [| 10.; 9.; 8. |]; [| 15.; 9.; 8. |] |] );
         ( "lebron blk mat" >:: fun _ ->
           assert_equal
             (Array.sub (fst p_lebron) 0 3)
             [|
               [| 0.; 1.; 1.; 0. |]; [| 2.; 2.; 1.; 1. |]; [| 1.; 1.; 0.; 2. |];
             |] );
         ( "gary stl mat" >:: fun _ ->
           assert_equal
             (Array.sub (fst p_gary) 0 3)
             [|
               [| 0.; 1.; 2.; 3.; 1. |];
               [| 0.; 1.; 2.; 1.; 0. |];
               [| 0.; 3.; 2.; 6.; 1. |];
             |] );
         ( "curry pts labels" >:: fun _ ->
           assert_equal (Array.sub (snd p_curry) 0 3) [| 47.; 34.; 30. |] );
         ( "wig reb labels" >:: fun _ ->
           assert_equal (Array.sub (snd p_wig) 0 3) [| 5.; 6.; 3. |] );
         ( "trae ast labels" >:: fun _ ->
           assert_equal (Array.sub (snd p_trae) 0 3) [| 15.; 16.; 10. |] );
         ( "lebron blk labels" >:: fun _ ->
           assert_equal (Array.sub (snd p_lebron) 0 3) [| 2.; 4.; 1. |] );
         ( "gary stl labels" >:: fun _ ->
           assert_equal (Array.sub (snd p_gary) 0 3) [| 1.; 1.; 0. |] );
       ]

(* Interpolated data *)
let i_curry =
  Processing.interpolated_data mat "Stephen Curry" [| "PTS"; "OPP"; "HOME" |] 1

let i_wig =
  Processing.interpolated_data mat "Andrew Wiggins" [| "REB"; "OPP"; "HOME" |] 2

let i_trae =
  Processing.interpolated_data mat "Trae Young" [| "AST"; "OPP"; "HOME" |] 3

let i_lebron =
  Processing.interpolated_data mat "LeBron James" [| "BLK"; "OPP"; "HOME" |] 4

let i_gary =
  Processing.interpolated_data mat "Gary Payton II" [| "STL"; "OPP"; "HOME" |] 5

let interp_data_tests =
  "test suite for interpolated data generation"
  >::: [
         ( "curry pts interp" >:: fun _ ->
           assert_equal (Array.sub i_curry 0 3)
             [|
               [| "47"; "14."; "1." |];
               [| "34"; "0."; "0." |];
               [| "30"; "7."; "0." |];
             |] );
         ( "wig reb interp" >:: fun _ ->
           assert_equal (Array.sub i_wig 0 3)
             [|
               [| "5"; "9."; "0." |];
               [| "6"; "12."; "0." |];
               [| "3"; "12."; "0." |];
             |] );
         ( "trae ast interp" >:: fun _ ->
           assert_equal (Array.sub i_trae 0 3)
             [|
               [| "15"; "4."; "0." |];
               [| "16"; "8."; "0." |];
               [| "10"; "13."; "1." |];
             |] );
         ( "lebron blk interp" >:: fun _ ->
           assert_equal (Array.sub i_lebron 0 3)
             [|
               [| "2"; "4."; "0." |];
               [| "4"; "11."; "0." |];
               [| "1"; "6."; "1." |];
             |] );
         ( "gary stl interp" >:: fun _ ->
           assert_equal (Array.sub i_gary 0 3)
             [|
               [| "1"; "8."; "1." |];
               [| "1"; "10."; "0." |];
               [| "0"; "7."; "1." |];
             |] );
       ]

(* Stacked data *)
let curry_stack =
  Processing.stack (Utils.float_to_string_mat (fst p_curry)) i_curry

let wig_stack = Processing.stack (Utils.float_to_string_mat (fst p_wig)) i_wig

let trae_stack =
  Processing.stack (Utils.float_to_string_mat (fst p_trae)) i_trae

let lebron_stack =
  Processing.stack (Utils.float_to_string_mat (fst p_lebron)) i_lebron

let gary_stack =
  Processing.stack (Utils.float_to_string_mat (fst p_gary)) i_gary

let stack_tests =
  "test suite for data stacking"
  >::: [
         ( "curry pts stack" >:: fun _ ->
           assert_equal
             (Array.sub curry_stack 0 3)
             [|
               [| "3."; "47"; "14."; "1." |];
               [| "8."; "34"; "0."; "0." |];
               [| "15."; "30"; "7."; "0." |];
             |] );
         ( "wig reb stack" >:: fun _ ->
           assert_equal (Array.sub wig_stack 0 3)
             [|
               [| "2."; "4."; "5"; "9."; "0." |];
               [| "2."; "8."; "6"; "12."; "0." |];
               [| "2."; "4."; "3"; "12."; "0." |];
             |] );
         ( "trae ast stack" >:: fun _ ->
           assert_equal (Array.sub trae_stack 0 3)
             [|
               [| "11."; "8."; "11."; "15"; "4."; "0." |];
               [| "10."; "9."; "8."; "16"; "8."; "0." |];
               [| "15."; "9."; "8."; "10"; "13."; "1." |];
             |] );
         ( "lebron blk stack" >:: fun _ ->
           assert_equal
             (Array.sub lebron_stack 0 3)
             [|
               [| "0."; "1."; "1."; "0."; "2"; "4."; "0." |];
               [| "2."; "2."; "1."; "1."; "4"; "11."; "0." |];
               [| "1."; "1."; "0."; "2."; "1"; "6."; "1." |];
             |] );
         ( "gary stl stack" >:: fun _ ->
           assert_equal (Array.sub gary_stack 0 3)
             [|
               [| "0."; "1."; "2."; "3."; "1."; "1"; "8."; "1." |];
               [| "0."; "1."; "2."; "1."; "0."; "1"; "10."; "0." |];
               [| "0."; "3."; "2."; "6."; "1."; "0"; "7."; "1." |];
             |] );
       ]

(* Running all the tests *)
let _ = run_test_tt_main basic_tests
let _ = run_test_tt_main match_tests
let _ = run_test_tt_main player_stat_tests
let _ = run_test_tt_main period_data_tests
let _ = run_test_tt_main interp_data_tests
let _ = run_test_tt_main stack_tests
