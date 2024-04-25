open Owl

let theta x y =
  (* Convert input data to matrix and vector *)
  let x_mat = Mat.transpose (Mat.of_arrays x) in
  let y_vec = Mat.transpose (Mat.of_array y 1 (Array.length y)) in

  (* Print the shapes of x_mat and y_vec *)
  Printf.printf "Shape of x_mat: (%d, %d)\n"
    (fst (Mat.shape x_mat))
    (snd (Mat.shape x_mat));
  Printf.printf "Shape of y_vec: (%d, %d)\n"
    (fst (Mat.shape y_vec))
    (snd (Mat.shape y_vec));

  (* Matrix computations *)
  let xt = Mat.transpose x_mat in
  (* Print the shapes of all intermediate matrices *)
  Printf.printf "Shape of xt: (%d, %d)\n"
    (fst (Mat.shape xt))
    (snd (Mat.shape xt));
  let xtx = Mat.dot xt x_mat in
  Printf.printf "Shape of xtx: (%d, %d)\n"
    (fst (Mat.shape xtx))
    (snd (Mat.shape xtx));
  let xtx_inv = Mat.inv xtx in
  Printf.printf "Shape of xtx_inv: (%d, %d)\n"
    (fst (Mat.shape xtx_inv))
    (snd (Mat.shape xtx_inv));
  let xty = Mat.dot xt y_vec in

  Printf.printf "Shape of xty: (%d, %d)\n"
    (fst (Mat.shape xty))
    (snd (Mat.shape xty));

  (* Final matrix multiplication to compute regression coefficients *)
  Mat.dot xtx_inv xty
