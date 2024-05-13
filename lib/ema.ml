let simple_average (data : Column.t) =
  Column.fold_left (fun (x : float) y : float -> float_of_string y +. x) 0. data
  /. float_of_int (Column.size data)

let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t

let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t

let simple_average_list lst = sum lst / length lst

let weighted_average (data : Column.t) epsilon =
  Column.fold_left
    (fun x y -> ((1. -. epsilon) *. x) +. (epsilon *. float_of_string y))
    (float_of_string (Column.get data 0))
    (Column.sub data 1 (data.size - 1))

let rec weighted_average_list lst epsilon =
  match lst with
  | [ h ] -> h
  | h :: t ->
      ((1. -. epsilon) *. h) +. (epsilon *. weighted_average_list t epsilon)
  | [] -> 0.

let rec simple_moving_average_aux data acc index =
  if index < Column.size data then
    simple_moving_average_aux data
      (((List.hd acc *. float_of_int index)
       +. float_of_string (Column.get data index))
       /. (float_of_int index +. 1.)
      :: acc)
      (index + 1)
  else acc

let simple_moving_average data =
  simple_moving_average_aux data (float_of_string (Column.get data 0) :: []) 1

let rec weighted_moving_average_aux data epsilon acc index =
  if index < Column.size data then
    weighted_moving_average_aux data epsilon
      ((epsilon *. List.hd acc)
       +. ((1. -. epsilon) *. float_of_string (Column.get data index))
      :: acc)
      (index + 1)
  else acc

let weighted_moving_average epsilon data =
  weighted_moving_average_aux data epsilon
    (float_of_string (Column.get data 0) :: [])
    1

let rec simple_moving_average_list_aux data acc index =
  match data with
  | [] -> acc
  | h :: t ->
      simple_moving_average_list_aux t
        ((h +. (float_of_int index *. List.hd acc))
         /. (float_of_int index +. 1.)
        :: acc)
        (index + 1)

let simple_moving_average_list data =
  if data = [] then []
  else
    List.rev
      (simple_moving_average_list_aux (List.tl data) (List.hd data :: []) 1)

let rec weighted_moving_average_list_aux data epsilon acc index =
  match data with
  | [] -> acc
  | h :: t ->
      weighted_moving_average_list_aux t epsilon
        ((((1. -. epsilon) *. h) +. (epsilon *. List.hd acc)) :: acc)
        (index + 1)

let weighted_moving_average_list epsilon data =
  if data = [] then []
  else
    List.rev
      (weighted_moving_average_list_aux (List.tl data) epsilon
         (List.hd data :: []) 1)

let data =
  Column.make
    [
      "10.0";
      "11.0";
      "12.0";
      "13.0";
      "14.0";
      "15.0";
      "16.0";
      "17.0";
      "18.0";
      "19.0";
    ]
    (Column.empty 10)

let demo1 = simple_moving_average data
let demo2 = weighted_moving_average 0.5 data
let smoothing = 2.
let period = 9

(* for a 10-day EMA *)
(* let () = List.iter (Printf.printf "%.2f\n") ema_result *)
