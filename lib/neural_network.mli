(** [activation] represents the activation function used in a layer. *)
type activation =
  | ReLU
  | Sigmoid
  | Tanh

(** [layer] represents a layer in the neural network. *)
type layer = Dense of int * activation

type node = {
  weights : Owl.Mat.mat;
  biases : Owl.Mat.mat;
  activation : activation;
}
(** [node] represents the parameters of a layer in the network. *)

type neural_network
(** [neural_network] represents a neural network structure. *)

val create : layer list -> int -> neural_network
(** [create layers input_dim] initializes a new neural network with the
    specified layers and input dimension. *)

val train : neural_network -> Owl.Mat.mat -> Owl.Mat.mat -> int -> float -> unit
(** [train nn x y epochs learning_rate] trains the neural network [nn] using
    data [x] and [y] for [epochs] number of epochs and the specified
    [learning_rate]. *)

val predict : neural_network -> Owl.Mat.mat -> Owl.Mat.mat
(** [predict nn x] returns the predictions of the neural network [nn] given
    input data [x]. *)
