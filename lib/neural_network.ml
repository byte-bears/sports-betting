open Owl
open List

(** Type definitions *)

type activation =
  | ReLU
  | Sigmoid
  | Tanh

type layer = Dense of int * activation

type node = {
  weights : Owl.Mat.mat;
  biases : Owl.Mat.mat;
  activation : activation;
}

type neural_network = {
  layers : layer list;
  nodes : node array;
}

(** Utility functions for activation *)

let relu x = if x > 0. then x else 0.
let relu_derivative x = if x > 0. then 1. else 0.
let sigmoid x = 1.0 /. (1.0 +. exp (-.x))

let sigmoid_derivative x =
  let sig_x = sigmoid x in
  sig_x *. (1. -. sig_x)

let tanh x =
  let e_pos = exp x and e_neg = exp (-.x) in
  (e_pos -. e_neg) /. (e_pos +. e_neg)

let tanh_derivative x =
  let tanh_x = tanh x in
  1. -. (tanh_x *. tanh_x)

let apply_activation x = function
  | ReLU -> relu x
  | Sigmoid -> sigmoid x
  | Tanh -> tanh x

let apply_activation_derivative x = function
  | ReLU -> relu_derivative x
  | Sigmoid -> sigmoid_derivative x
  | Tanh -> tanh_derivative x
