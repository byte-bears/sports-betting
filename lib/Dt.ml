open Csv
(* open Column *)

exception EmptyTable

type t = {
  mutable headers : string array;
  mutable dt : Column.t array;
  mutable size : int; (* number of cols *)
}

let empty col row =
  {
    headers = Array.make col " ";
    dt = Array.make col (Column.empty row);
    size = 0;
  }

let add table name (col : Column.t) =
  let () = table.headers.(table.size) <- name in
  let () = table.dt.(table.size) <- col in
  table.size <- table.size + 1

let rec load_csv_helper data acc =
  match data with
  | [] -> acc
  | h :: t -> begin
      let h_arr = Array.of_list h in
      add acc (List.nth h 0) { data = h_arr; size = Array.length h_arr };
      load_csv_helper t acc
    end

let load_csv file_path =
  let data_loaded = load file_path |> transpose in
  let acc =
    {
      headers = Array.make (List.length data_loaded) "";
      dt = Array.make (List.length data_loaded) (Column.empty 0);
      (* dt = Array.make (List.length data_loaded) { data = [||]; size = 0 }; *)
      size = 0;
    }
  in
  load_csv_helper data_loaded acc

(* let rec make_helper table (row : int) names = function | [] -> () | h :: t ->
   begin match names with | [] -> () | name :: t2 -> let () = add table name
   (Column.make h (Column.empty row)) in make_helper table row t2 t end

   let make file names = let columns = Csv.transpose (Csv.load file) in let rows
   = List.length (List.nth columns 0) in make_helper c rows names columns *)
