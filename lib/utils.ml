let strip_str str =
  let n = String.length str in
  if n = 0 then ""
  else
    let ret = Str.replace_first (Str.regexp "^[ \t\n]+") "" str in
    Str.replace_first (Str.regexp "[ \t\n]+$") "" ret

let rec print_list ?(interp = " ") lst =
  match lst with
  | [] -> Printf.printf "\n"
  | h :: t ->
      Printf.printf "%s%s" h interp;
      print_list t ~interp

let float_to_string_arr arr =
  let new_arr = Array.make (Array.length arr) "" in
  for i = 0 to Array.length arr - 1 do
    new_arr.(i) <- string_of_float arr.(i)
  done;
  new_arr

let print_arr ?(interp = " ") arr =
  for i = 0 to Array.length arr - 1 do
    Printf.printf "%s%s" arr.(i) interp
  done;
  print_newline ()

let print_col_lengths mat =
  let lengths = Array.map Array.length mat in
  for i = 0 to Array.length lengths - 1 do
    Printf.printf "%s %d, " mat.(i).(0) lengths.(i)
  done;
  print_newline ()

let print_length arr =
  let l = Array.length arr in
  Printf.printf "%d\n" l

let shape mat =
  let n_cols = Array.length mat in
  if n_cols = 0 then (0, 0)
  else
    let n_rows = Array.length mat.(0) in
    (n_rows, n_cols)

let print_shape mat =
  let x, y = shape mat in
  Printf.printf "%d x %d\n" x y

let float_to_string_mat mat =
  let new_mat =
    Array.make_matrix (Array.length mat) (Array.length mat.(0)) ""
  in
  for i = 0 to Array.length mat - 1 do
    for j = 0 to Array.length mat.(0) - 1 do
      new_mat.(i).(j) <- string_of_float mat.(i).(j)
    done
  done;
  new_mat

let print_mat mat =
  match shape mat with
  | _, 0 -> Printf.printf "[]\n"
  | 0, _ -> Printf.printf "[]\n"
  | _, _ ->
      for i = 0 to Array.length mat.(0) - 1 do
        let () = Printf.printf "|" in
        for j = 0 to Array.length mat - 1 do
          Printf.printf " %s |" mat.(j).(i)
        done;
        Printf.printf "\n"
      done

let print_mat_colwise mat =
  for i = 0 to Array.length mat - 1 do
    let () = Printf.printf "|" in
    for j = 0 to Array.length mat.(0) - 1 do
      Printf.printf " %s |" mat.(i).(j)
    done;
    Printf.printf "\n"
  done

let transpose mat =
  let n_cols, n_rows = shape mat in
  let new_mat = Array.make_matrix n_cols n_rows "" in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      new_mat.(j).(i) <- mat.(i).(j)
    done
  done;
  new_mat
