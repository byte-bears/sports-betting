open Sports_betting

let mat = Load.load_string_array "data/boxscores.csv"
let () = Load.make_rectangular_cols mat ""
let period = 5

(* let small = Load.filter_cols mat [| "PLAYER_NAME"; "GAME_DATE"; "MATCHUP";
   "OPP"; "HOME" |]

   let () = Utils.print_mat small *)
let mat1 =
  Processing.get_player_stats mat "Draymond Green"
    [| "PLAYER_NAME"; "PTS"; "HOME"; "OPP" |]

let () = Utils.print_mat mat1

let mat2 =
  Processing.period_data Processing.get_player_stat mat "Draymond Green" "PTS"
    period

let () = Utils.print_mat_colwise (Utils.float_to_string_mat (fst mat2))
let () = Utils.print_arr (Utils.float_to_string_arr (snd mat2))
let () = Utils.print_shape (fst mat2)

let mat3 =
  Processing.interpolated_data mat "Draymond Green" [| "PTS"; "OPP"; "HOME" |]
    period

let () = Utils.print_mat_colwise mat3
let () = Utils.print_shape mat3
let ultimate = Processing.stack (Utils.float_to_string_mat (fst mat2)) mat3
let () = Utils.print_mat_colwise ultimate
let good = Processing.good_features mat "Draymond Green" "PTS"
let () = print_endline (Datatable.to_string (fst good))
