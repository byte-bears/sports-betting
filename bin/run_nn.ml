open Owl
open Neural_network

let () =
  (* Step 1: Define the network architecture *)
  let layers = [ Dense (3, ReLU); Dense (2, Sigmoid) ] in
  (* Simple network with one hidden layer *)
  let input_dim = 2 in
  (* Each input vector has two elements *)
  let network = create layers input_dim in

  (* Step 2: Generate dummy data *)
  (* Let's say we have 4 samples, each sample is of input_dim, and we are trying
     to predict 2 outputs *)
  let inputs = List.init 4 (fun _ -> Mat.uniform 1 2) in
  (* 4 input samples of dimension 1x2 *)
  let actuals = List.init 4 (fun _ -> Mat.uniform 1 2) in

  (* 4 target output samples of dimension 1x2 *)

  (* Step 3: Train the network *)
  let epochs = 1000 in
  let learning_rate = 0.01 in
  let trained_network = train network inputs actuals epochs learning_rate in

  (* Step 4: Evaluate the network *)
  List.iter
    (fun input ->
      let output = forward trained_network input in
      Printf.printf "Input:\n";
      Mat.print input;
      Printf.printf "Predicted Output:\n";
      Mat.print output)
    inputs
