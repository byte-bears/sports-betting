open Sports_betting
open OUnit2

let features_arr =
  [|
    (* "PLAYER_ID"; "GAME_ID"; "GAME_DATE"; *)
    (* "WL"; *)
    "MIN";
    "FGM";
    "FGA";
    "FG_PCT";
    "FG3M";
    "FG3A";
    "FG3_PCT";
    "FTM";
    "FTA";
    "FT_PCT";
    "OREB";
    "DREB";
    "REB";
    "AST";
    "STL";
    "BLK";
    "TOV";
    "PF";
    "PLUS_MINUS";
    "FANTASY_PTS";
  |]

let output = "PTS"

(** USAGE *)
let entire_data = Load.load_string_array "data/boxscores_mini.csv"

let x = Load.filter_cols entire_data features_arr |> Load.load_float_array
let x_rect = Load.make_rectangular x
let x_ones = Load.add_col x_rect (Array.make (Array.length x_rect.(0)) 1.)
let _ = assert_equal true (Load.is_rectangular x_rect)
let _ = assert_equal true (Load.is_rectangular x_ones)
let y = Load.get_col entire_data output |> Load.load_float_col
let params = Linear_regression.theta x_ones y
let () = Owl.Mat.print params
