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
  activation : activation;
}

type neural_network = {
  layers : layer list;
  layer_nodes : node list;
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
  | ReLU -> Mat.map relu x
  | Sigmoid -> Mat.map sigmoid x
  | Tanh -> Mat.map tanh x

let apply_activation_derivative x = function
  | ReLU -> Mat.map relu_derivative x
  | Sigmoid -> Mat.map sigmoid_derivative x
  | Tanh -> Mat.map tanh_derivative x

(** Initialization of weights and biases *)

let initialize_weights input_dim output_dim =
  let scale = sqrt (2. /. float_of_int input_dim) in
  Mat.gaussian ~mu:0. ~sigma:scale input_dim output_dim

(** Creating a new neural network *)

let create_node input_dim output_dim activation =
  { weights = initialize_weights input_dim output_dim; activation }

let create (layers : layer list) input_dim =
  let rec build_nodes (layers : layer list) input_dim acc_nodes =
    match layers with
    | [] -> List.rev acc_nodes
    | Dense (num_nodes, activation) :: rest ->
        let node = create_node input_dim num_nodes activation in
        build_nodes rest num_nodes (node :: acc_nodes)
  in
  let nodes_lst = build_nodes layers input_dim [] in
  { layers; layer_nodes = nodes_lst }

(** Forward pass *)

let forward_node input node =
  let input_features = Mat.col_num input in
  let weight_inputs = Mat.col_num node.weights in

  if input_features <> weight_inputs then
    failwith
      (Printf.sprintf
         "Dimension mismatch: input features %d do not match weight inputs %d"
         input_features weight_inputs)
  else
    let z = Mat.(input *@ transpose node.weights) in
    apply_activation z node.activation

let forward network input =
  let rec propagate nodes input acc =
    match nodes with
    | [] -> List.rev acc
    | node :: rest ->
        let output = forward_node input node in
        propagate rest output (output :: acc)
  in
  propagate network.layer_nodes input []

(** Backprop *)

let backward_pass network input actual =
  let rec backprop nodes outputs grads =
    match (nodes, outputs) with
    | [], _ -> grads
    | node :: rest_nodes, output :: prev_outputs ->
        let input_to_node =
          match prev_outputs with
          | prev_output :: _ -> prev_output
          | [] ->
              (* Return a zero matrix that matches the expected dimension *)
              let num_samples =
                Mat.row_num output
                (* Assuming 'output' has correct number of rows for samples *)
              and input_features = Mat.col_num node.weights in
              (* Use the weight matrix to determine size *)
              Mat.zeros num_samples input_features
        in
        let error = Mat.(output - actual) in
        let activation_deriv =
          apply_activation_derivative output node.activation
        in
        let delta = Mat.(activation_deriv * error) in
        let grad_w = Mat.(transpose input_to_node *@ delta) in
        let grad_input = Mat.(delta *@ transpose node.weights) in
        backprop rest_nodes prev_outputs ((grad_w, grad_input) :: grads)
    | _ -> failwith ""
  in
  let outputs = forward network input in
  backprop (List.rev network.layer_nodes) (List.rev outputs) []

(** Updates *)

let update_weights node grad_w learning_rate =
  { node with weights = Mat.(node.weights - (learning_rate $* grad_w)) }

let update_network network grads learning_rate =
  let new_layer_nodes =
    List.map2
      (fun node (grad_w, _) -> update_weights node grad_w learning_rate)
      network.layer_nodes grads
  in
  { network with layer_nodes = new_layer_nodes }

(** Training loop *)

let train network inputs actuals epochs learning_rate =
  let rec loop network epoch =
    if epoch > epochs then network
    else
      let updated_network =
        List.fold_left2
          (fun net input actual ->
            let grads = backward_pass net input actual in
            update_network net grads learning_rate)
          network inputs actuals
      in
      loop updated_network (epoch + 1)
  in
  loop network 1
