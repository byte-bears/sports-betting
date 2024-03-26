exception Not_found
exception Out_of_bounds

type option =
  | Some of float
  | None

type t = {
  mutable data : option array;
  mutable size : int;
}

let empty capacity = { data = Array.make capacity None; size = 0 }
let size col = col.size

let add col elt =
  let () = col.data.(col.size) <- elt in
  col.size <- col.size + 1

let get col idx =
  if idx >= size col then raise Out_of_bounds
  else
    match col.data.(idx) with
    | None -> raise Not_found
    | Some x -> x

let rec make lst col =
  if List.length lst <> size col then raise Out_of_bounds
  else
    match lst with
    | [] -> col
    | h :: t ->
        let () = add col (Some h) in
        make t col

let rec make_string lst col =
  if List.length lst <> size col then raise Out_of_bounds
  else
    match lst with
    | [] -> col
    | h :: t ->
        let () = add col (Some (float_of_string h)) in
        make_string t col
