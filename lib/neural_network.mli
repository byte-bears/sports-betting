type activation =
  | ReLU
  | Sigmoid
  | Tanh
      (** Type representing different activation functions for neural networks. *)

type layer =
  | Dense of int * activation
      (** The type of a layer in the neural network. It consists of the number
          of nodes in the layer and the activation function used. *)

type node = {
  weights : Owl.Mat.mat;  (** The weights associated with the node. *)
  activation : activation;  (** The activation function to be used. *)
}
(** The type of a node in the neural network. It contains the weights associated
    with the node and the activation function used. *)

type neural_network = {
  layers : layer list;  (** The layers in the neural network. *)
  layer_nodes : node list;  (** The nodes in the neural network. *)
}
(** The type of a neural network. It contains a list of layers and a list of
    nodes. *)

val create : layer list -> int -> neural_network
(** [create layers input_size] creates a neural network with [layers] as the
    list of layers and [input_size] as the number of dimensions. *)

val forward :
  neural_network ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t ->
  Owl.Mat.mat list
(** [forward network input] performs forward propagation on the given neural
    network [network] with the given [input]. It returns a list of matrices
    representing the activations of each layer. *)

val train :
  neural_network ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t list ->
  (float, Bigarray.float64_elt) Owl_dense_matrix_generic.t list ->
  int ->
  float ->
  neural_network
(** [train network inputs targets epochs learning_rate] trains the neural
    network [network] with the given [inputs] and [targets] (expected values)
    for the specified number of [epochs] using the given [learning_rate]. It
    returns the trained neural network. *)
