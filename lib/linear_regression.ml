open Owl

let array_to_matrix data = Mat.of_arrays data

let manual_hstack x ones =
  let m = Mat.row_num x in
  let n1 = Mat.col_num x in
  let n2 = Mat.col_num ones in
  let result = Mat.empty m (n1 + n2) in
  for i = 0 to m - 1 do
    for j = 0 to n1 - 1 do
      Mat.set result i j (Mat.get x i j)
    done;
    for j = 0 to n2 - 1 do
      Mat.set result i (j + n1) (Mat.get ones i j)
    done
  done;
  result

let linear_regression x y =
  let ones = Mat.ones (Mat.row_num x) 1 in
  let x' = manual_hstack x ones in
  let xt = Mat.transpose x' in
  let xtx = Mat.dot xt x' in
  let xty = Mat.dot xt y in
  let beta = Mat.dot (Mat.inv xtx) xty in
  beta

let of_col_vec arr =
  let n = Array.length arr in
  (* Number of rows in the resulting column matrix *)
  let m = Mat.empty n 1 in
  (* Initialize an empty matrix with n rows and 1 column *)
  Array.iteri (fun i x -> Mat.set m i 0 x) arr;
  (* Fill the matrix with elements from the array *)
  m

let features_arr =
  [|
    "PLAYER_ID";
    "GAME_ID";
    "GAME_DATE";
    "WL";
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
let entire_data = Load.load_string_array "data/boxscores.csv"

let x = Load.filter_cols entire_data features_arr |> Load.load_float_array
let y = Load.get_col entire_data output |> Load.load_float_col

(* Convert arrays directly to matrices *)
(* let x_matrix = array_to_matrix x in let y_matrix = of_col_vec y in

   (* Perform linear regression *) let beta = linear_regression x_matrix
   y_matrix in

   (* Output the coefficients *) Mat.print beta *)
