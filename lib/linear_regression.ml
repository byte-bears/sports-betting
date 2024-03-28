let rec populate_column_helper data curr_columns =
  match data with
  | [] -> List.rev curr_columns
  | h :: t -> populate_column_helper t (h :: curr_columns)

let populate_column data = populate_column_helper data []

let load_data_string path : string list list =
  let columns : string list list = Csv.load path in
  Csv.square (Csv.transpose (populate_column columns))

let load_csv_float csv_file =
  let data : string list list = load_data_string csv_file in
  let filtered_data =
    List.map
      (fun row ->
        List.filter_map
          (fun item -> try Some (float_of_string item) with Failure _ -> None)
          row)
      data
  in
  filtered_data

let ndarray = load_data_string "data/test.csv"
let test = load_csv_float "data/test.csv"
