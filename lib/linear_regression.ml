open Torch
open Datatable
open Column

let theta x y =
  (* Convert input data to matrix and vector *)
  let y = [| y |] in
  let x_mat = Tensor.transpose (Tensor.of_float2 x) ~dim0:0 ~dim1:1 in
  let y_vec = Tensor.transpose (Tensor.of_float2 y) ~dim0:0 ~dim1:1 in

  (* Print the shapes of x_mat and y_vec *)
  let x_shape = Array.of_list (Tensor.shape x_mat) in
  let y_shape = Array.of_list (Tensor.shape y_vec) in

  (* Printf.printf "Shape of x_mat: (%d, %d)\n" x_shape.(0) x_shape.(1); *)
  (* Printf.printf "Shape of y_vec: (%d, %d)\n" y_shape.(0) y_shape.(1); *)

  (* Matrix computations *)
  let xt = Tensor.transpose x_mat ~dim0:0 ~dim1:1 in
  (* Print the shapes of all intermediate matrices *)
  let xt_shape = Array.of_list (Tensor.shape xt) in
  (* Printf.printf "Shape of xt: (%d, %d)\n" xt_shape.(0) xt_shape.(1); *)
  let xtx = Tensor.matmul xt x_mat in
  let xtx_shape = Array.of_list (Tensor.shape xtx) in
  (* Printf.printf "Shape of xtx: (%d, %d)\n" xtx_shape.(0) xtx_shape.(1); *)
  let xtx_inv = Tensor.inverse xtx in
  let xtx_inv_shape = Array.of_list (Tensor.shape xtx_inv) in
  (* Printf.printf "Shape of xtx_inv: (%d, %d)\n" xtx_inv_shape.(0)
     xtx_inv_shape.(1); *)
  let xty = Tensor.matmul xt y_vec in
  let xty_shape = Array.of_list (Tensor.shape xty) in

  (* Printf.printf "Shape of xty: (%d, %d)\n" xty_shape.(0) xty_shape.(1); *)

  (* Final matrix multiplication to compute regression coefficients *)
  Tensor.matmul xtx_inv xty

let theta_datatable datatable column =
  theta (to_float_array datatable) (to_float_column column)
