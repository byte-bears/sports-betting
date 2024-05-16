exception Out_of_bounds

(* AF: The mutable string array data [[s1; ... ; sn]] represents a column and
   where s1; ... ; sn are the string representations of the contents of the
   column. *)
(* RI: none*)
type t = {
  mutable data : string array;
  mutable size : int;
}

let empty capacity = { data = Array.make capacity ""; size = 0 }
let size col = col.size
let capacity col = Array.length col.data

let add col elt =
  col.data.(col.size) <- elt;
  col.size <- col.size + 1

let remove col =
  if size col = 0 then raise Out_of_bounds else col.data.(col.size - 1) <- "";
  col.size <- col.size - 1

let get col idx =
  if idx >= size col then raise Out_of_bounds else col.data.(idx)

let rec make lst col =
  (*if List.length lst <> Array.length col.data then raise Out_of_bounds else*)
  match lst with
  | [] -> col
  | h :: t ->
      let () = add col h in
      make t col

let make_from_array array col =
  let col = empty (Array.length array + 1) in
  for i = 0 to Array.length array do
    col.data.(i) <- array.(i)
  done;
  col

let fold_left f x y = Array.fold_left f x y.data

let sub col (pos : int) (len : int) =
  let new_col = empty (size col) in
  new_col.data <- Array.sub col.data pos len;
  new_col.size <- len;
  new_col

let extend col new_size : t =
  if Array.length col.data >= new_size then col
  else
    let extra_size = new_size - Array.length col.data in
    let extra_elements = Array.make extra_size "" in
    let new_data = Array.append col.data extra_elements in
    { data = new_data; size = col.size }

let string_of_array_with_spaces n arr =
  let rec spaces m = if m <= 0 then "" else " " ^ spaces (m - 1) in
  let with_spaces elem =
    let spaces_needed = n - String.length elem in
    elem ^ spaces spaces_needed
  in
  let concatenated =
    Array.fold_left
      (fun acc elem ->
        let elem_with_spaces = with_spaces elem in
        if acc = "" then elem_with_spaces else acc ^ "|" ^ elem_with_spaces)
      "" arr
  in
  String.trim concatenated

let max_length_arr arr =
  let rec find_max idx current_max =
    match idx with
    | _ when idx >= Array.length arr -> current_max
    | _ ->
        let len = String.length arr.(idx) in
        let new_max = if len > current_max then len else current_max in
        find_max (idx + 1) new_max
  in
  find_max 0 0

let to_string n arr =
  if Array.length arr.data > n then raise Out_of_bounds
  else string_of_array_with_spaces n arr.data

let max_length arr = max_length_arr arr.data

let to_float_column col =
  let to_ret = Array.make (capacity col) 0. in
  for i = 0 to col.size - 1 do
    to_ret.(i) <- (try float_of_string col.data.(i) with _ -> 0.)
  done;
  to_ret
