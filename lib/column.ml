exception Out_of_bounds

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

let rec to_string_list lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ " " ^ to_string_list t

let to_string arr = to_string (Array.to_list arr.data)

let string_of_array_with_spaces n arr =
  let rec spaces n = if n <= 0 then "" else " " ^ spaces (n - 1) in
  let space_str = spaces n in
  Array.fold_left
    (fun acc elem -> if acc = "" then elem else acc ^ space_str ^ elem)
    "" arr
