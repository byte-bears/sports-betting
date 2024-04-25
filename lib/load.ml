open OUnit2

let load_string_array filepath : string array array =
  let csv_data = Csv.transpose (Csv.load filepath) in
  let arrays = List.map Array.of_list csv_data in
  Array.of_list arrays

let load_float_col arr =
  Array.of_list
    (List.filter_map
       (fun str -> try Some (float_of_string str) with Failure _ -> None)
       (Array.to_list arr))

let load_float_array arr = Array.map load_float_col arr

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

let is_rectangular arr =
  if Array.length arr = 0 then true
  else
    let first_col_length = Array.length arr.(0) in
    Array.for_all (fun col -> Array.length col = first_col_length) arr

let max_length arr =
  Array.fold_left (fun acc row -> max acc (Array.length row)) 0 arr

let make_rectangular arr =
  let max_len = max_length arr in
  Array.map
    (fun row ->
      if Array.length row < max_len then
        Array.append row (Array.make (max_len - Array.length row) 0.0)
      else row)
    arr
