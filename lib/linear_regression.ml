open Torch
open Datatable
open Column

let theta x y =
  (* Convert input data to matrix and vector *)
  let y = [| y |] in
  let x_mat = Tensor.transpose (Tensor.of_float2 x) ~dim0:0 ~dim1:1 in
  let y_vec = Tensor.transpose (Tensor.of_float2 y) ~dim0:0 ~dim1:1 in

  (* Matrix computations *)
  let xt = Tensor.transpose x_mat ~dim0:0 ~dim1:1 in
  let xtx = Tensor.matmul xt x_mat in
  let xtx_inv = Tensor.inverse xtx in
  let xty = Tensor.matmul xt y_vec in

  Tensor.matmul xtx_inv xty

let theta_datatable datatable column =
  theta (to_float_array datatable) (to_float_column column)
