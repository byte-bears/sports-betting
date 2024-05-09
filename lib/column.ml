exception Not_found
exception Out_of_bounds

type t = {
  mutable data : string array;
  mutable size : int;
}

let empty capacity = { data = Array.make capacity ""; size = 0 }
let size col = col.size

let add col elt =
  col.data.(col.size) <- elt;
  col.size <- col.size + 1

let remove col =
  if size col = 0 then raise Out_of_bounds else col.data.(col.size) <- "";
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
