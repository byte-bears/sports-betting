let simple_moving_average (days : Column.t) =
  Column.fold_left (fun (x : float) y : float -> float_of_string y +. x) 0. days
  /. float_of_int (Column.size days)

let data = [ 10.0; 11.0; 12.0; 13.0; 14.0; 15.0; 16.0; 17.0; 18.0; 19.0 ]
let smoothing = 2.
let period = 9
(* for a 10-day EMA *)
(* let () = List.iter (Printf.printf "%.2f\n") ema_result *)
