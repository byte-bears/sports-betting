exception Uneven_Table
exception Out_of_bounds

type t = {
  mutable headers : string array;
  mutable dt : Column.t array;
  mutable size : int;
}

let col_num table = Array.length table.dt
let row_num table = Column.capacity table.dt.(0)

let empty col row =
  {
    headers = Array.make col " ";
    dt = Array.make col (Column.empty row);
    size = 0;
  }

let add_helper table name (col : Column.t) =
  let () = table.headers.(table.size) <- name in
  let () = table.dt.(table.size) <- col in
  table.size <- table.size + 1

let add table name (col : Column.t) =
  if Array.length col.data > Column.capacity table.dt.(0) then
    add_helper table name (Column.sub col 0 (Column.capacity table.dt.(0)))
  else if Array.length col.data < Column.capacity table.dt.(0) then
    add_helper table name (Column.extend col (Column.capacity table.dt.(0)))
  else add_helper table name col

let max_length_arr dt =
  let max_len = ref 0 in
  for i = 0 to Array.length dt - 1 do
    let len = Column.max_length dt.(i) in
    if len > !max_len then max_len := len
  done;
  !max_len

let max_length dt = max_length_arr dt.dt

let to_string dt =
  let max =
    max (max_length_arr dt.dt + 1) (Column.max_length_arr dt.headers + 1)
  in
  let str = ref "" in
  let rec spaces m = if m <= 0 then "" else " " ^ spaces (m - 1) in
  for i = 0 to Array.length dt.dt - 1 do
    str :=
      !str ^ "\n" ^ dt.headers.(i)
      ^ spaces (max - String.length dt.headers.(i))
      ^ "||"
      ^ Column.to_string max dt.dt.(i)
  done;
  !str

let to_float_array (table : t) =
  let to_ret = Array.make (col_num table) [| 0. |] in
  for i = 0 to table.size do
    to_ret.(i) <- Column.to_float_column table.dt.(i)
  done;
  to_ret

let make (matrix : string array array) (names : string array) =
  let col_nums = Array.length matrix in
  let row_nums = Array.length matrix.(0) in
  let to_ret = empty col_nums row_nums in
  for i = 0 to col_nums do
    to_ret.dt.(i) <- Column.make_from_array matrix.(i) (Column.empty row_nums)
  done;
  to_ret.headers <- names;
  to_ret
