let exponential_moving_average smoothing period data =
  let alpha = smoothing /. (1. +. float_of_int period) in
  let rec ema_aux data prev_ema acc =
    match data with
    | [] -> List.rev acc
    | h :: t ->
        let ema = (h *. alpha) +. (prev_ema *. (1. -. alpha)) in
        ema_aux t ema (ema :: acc)
  in
  match data with
  | [] -> []
  | h :: t -> ema_aux t h [ h ]

let data = [ 10.0; 11.0; 12.0; 13.0; 14.0; 15.0; 16.0; 17.0; 18.0; 19.0 ]
let smoothing = 2.
let period = 9 (* for a 10-day EMA *)
let ema_result = exponential_moving_average smoothing period data
(* let () = List.iter (Printf.printf "%.2f\n") ema_result *)
