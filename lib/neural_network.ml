open Owl
open List

(** Type definitions *)

type activation =
  | ReLU
  | Sigmoid
  | Tanh

type layer = Dense of int * activation

type node = {
  weights : Mat.mat;
  biases : Mat.mat;
  activation : activation;
}

type neural_network = {
  layers : layer list;
  nodes : node list;
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

(** Initialization of weights and biases *)

let initialize_weights input_dim output_dim =
  let scale = sqrt (2. /. float_of_int input_dim) in
  Mat.gaussian ~mu:0. ~sigma:scale input_dim output_dim

let initialize_biases output_dim = Mat.zeros 1 output_dim

(** Creating a new neural network *)

let create_node input_dim output_dim activation =
  {
    weights = initialize_weights input_dim output_dim;
    biases = initialize_biases output_dim;
    activation;
  }

(** Creating a new neural network *)

let create (layers : layer list) input_dim =
  let rec build_nodes (layers : layer list) input_dim acc_nodes =
    match layers with
    | [] -> List.rev acc_nodes
    | Dense (num_nodes, activation) :: rest ->
        let node = create_node input_dim num_nodes activation in
        build_nodes rest num_nodes (node :: acc_nodes)
  in
  let nodes_lst = build_nodes layers input_dim [] in
  { layers; nodes = nodes_lst }

(** Forward pass *)

let forward nn x =
  let rec compute nodes input activations =
    match nodes with
    | [] -> List.rev activations
    | { weights; biases; activation } :: rest_nodes ->
        let z = float_of_int Mat.((input *@ weights) + biases) in
        let a = apply_activation z activation in
        compute rest_nodes a (a :: activations)
  in
  let activations = compute nn.nodes x [ x ] in
  List.rev activations
