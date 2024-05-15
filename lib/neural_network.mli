type activation =
  | ReLU
  | Sigmoid
  | Tanh

type layer = Dense of int * activation

type node = {
  weights : Owl.Mat.mat;
  activation : activation;
}

type neural_network = {
  layers : layer list;
  layer_nodes : node list;
}

val relu : float -> float
val relu_derivative : float -> float
val sigmoid : float -> float
val sigmoid_derivative : float -> float
val tanh : float -> float
val tanh_derivative : float -> float
val apply_activation : Owl.Mat.mat -> activation -> Owl.Mat.mat
val apply_activation_derivative : Owl.Mat.mat -> activation -> Owl.Mat.mat
val initialize_weights : int -> int -> Owl.Mat.mat
val create_node : int -> int -> activation -> node
val create : layer list -> int -> neural_network

val forward_node :
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t ->
  node ->
  Owl.Mat.mat

val forward :
  neural_network ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t ->
  Owl.Mat.mat list

val backward_pass :
  neural_network ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t ->
  ((float, Bigarray.float64_elt) Owl_dense_matrix_generic.t
  * (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t)
  list

val update_weights :
  node ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t ->
  float ->
  node

val update_network :
  neural_network ->
  ((float, Bigarray.float64_elt) Owl_dense_matrix_generic.t * 'a) list ->
  float ->
  neural_network

val train :
  neural_network ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t list ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t list ->
  int ->
  float ->
  neural_network
