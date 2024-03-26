exception EmptyTable

type 'a t = {
  mutable headers : string array;
  mutable dt : Column.t array;
  mutable size : int;
}

let empty col row =
  {
    headers = Array.make col " ";
    dt = Array.make col (Column.empty row);
    size = 0;
  }

let add table name (col : Column.t) =
  let () = table.headers.(col.size) <- name in
  let () = table.dt.(col.size) <- col in
  col.size <- col.size + 1

let rec make_helper table (row : int) names = function
  | [] -> ()
  | h :: t -> begin
      match names with
      | [] -> ()
      | name :: t2 ->
          let () = add table name (Column.make_string h (Column.empty row)) in
          make_helper table row t2 t
    end

let make file names dt =
  let columns = Csv.transpose (Csv.load file) in
  let rows = List.length (List.nth columns 0) in
  make_helper dt rows names columns
