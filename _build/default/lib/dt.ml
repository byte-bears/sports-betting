open Column

exception EmptyTable

type t = {
  mutable headers : string list;
  mutable dt : Column.t list;
}

let empty = []

let is_empty table =
  match table.dt with
  | [] -> true
  | _ -> false

let get_rows table =
  match table.dt with
  | [] -> 0
  | h :: _ -> List.length h
