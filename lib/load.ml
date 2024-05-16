open OUnit2
open Batteries

let load_string_array filepath : string array array =
  let csv_data = Csv.transpose (Csv.load filepath) in
  let arrays = List.map Array.of_list csv_data in
  Array.of_list arrays

let load_float_col arr =
  Array.of_list
    (List.filter_map
       (fun str -> try Some (float_of_string str) with Failure _ -> None)
       (Array.to_list arr))

let load_float_array mat = Array.map load_float_col mat

let filter_cols data features_arr =
  let features_set =
    Array.fold_left
      (fun acc feature ->
        Hashtbl.add acc feature true;
        acc)
      (Hashtbl.create 32) features_arr
  in
  let headers = Array.map (fun col -> col.(0)) data in
  let indices_to_keep =
    Array.fold_left
      (fun acc (hdr, idx) ->
        if Hashtbl.mem features_set hdr then idx :: acc else acc)
      []
      (Array.mapi (fun idx hdr -> (hdr, idx)) headers)
    |> List.rev
  in
  let filtered_data =
    Array.map (fun idx -> data.(idx)) (Array.of_list indices_to_keep)
  in
  filtered_data

let rec add_if_true acc bools col ind =
  if ind = Array.length bools then acc
  else
    match bools.(ind) with
    | true -> add_if_true (col.(ind) :: acc) bools col (ind + 1)
    | false -> add_if_true acc bools col (ind + 1)

let get_val_inds col target =
  let f x = x = target in
  let arr = Array.map f col in
  arr.(0) <- true;
  let indices = Array.mapi (fun i _ -> i) arr in
  Array.of_list (List.rev (add_if_true [] arr indices 0))

let filter_by_ind col inds = Array.map (fun i -> col.(i)) inds

let filter_by_col (data : string array array) col_name target =
  (* Finding correct column *)
  let idx = ref None in
  for i = 0 to Array.length data - 1 do
    if data.(i).(0) = col_name then idx := Some i
  done;

  (* Start doing stuff *)
  match !idx with
  | None -> failwith "Column not found in data"
  | Some i ->
      let val_inds = get_val_inds data.(i) target in
      for i = 0 to Array.length data - 1 do
        data.(i) <- filter_by_ind data.(i) val_inds
      done;
      data

let get_col data col_name =
  let idx = ref None in
  for i = 0 to Array.length data - 1 do
    if data.(i).(0) = col_name then idx := Some i
  done;

  match !idx with
  | None -> [||]
  | Some i -> data.(i)

let binarize_col f column = Array.map f column

let add_col a b =
  let _ = assert_equal (Array.length a.(0) = Array.length b) true in
  let new_size = Array.length a + 1 in
  let result = Array.make new_size [||] in
  Array.blit a 0 result 0 (Array.length a);
  Array.set result (new_size - 1) b;
  result

let is_rectangular mat =
  if Array.length mat = 0 then true
  else
    let first_col_length = Array.length mat.(0) in
    Array.for_all (fun col -> Array.length col = first_col_length) mat

let max_length mat =
  let m_len = ref 0 in
  for i = 0 to Array.length mat - 1 do
    m_len := max !m_len (Array.length mat.(i))
  done;
  !m_len

let make_rectangular_rows mat =
  let max_len = max_length mat in
  Array.map
    (fun row ->
      if Array.length row < max_len then
        Array.append row (Array.make (max_len - Array.length row) (-1.0))
      else row)
    mat

let make_rectangular_cols mat fill =
  let max_len = max_length mat in
  for i = 0 to Array.length mat - 1 do
    let len = Array.length mat.(i) in
    if len < max_len then
      mat.(i) <- Array.append mat.(i) (Array.make (max_len - len) fill)
  done
