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
