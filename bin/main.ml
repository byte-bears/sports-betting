open Owl
open Sports_betting.Neural_network

let () =
  (* Step 1: Define the network architecture *)
  let layers = [ Dense (1, ReLU); Dense (1, Sigmoid) ] in
  (* Simple network with one hidden layer *)
  let input_dim = 1 in
  let network = create layers input_dim in
  let inputs = List.init 4 (fun _ -> Mat.uniform 1 1) in
  let actuals = List.init 4 (fun _ -> Mat.uniform 1 1) in

  let epochs = 1000 in
  let learning_rate = 0.01 in
  let trained_network = train network inputs actuals epochs learning_rate in

  List.iter
    (fun input ->
      let output = forward trained_network input in
      Printf.printf "Predicted Output:\n";
      List.iter Mat.print output)
    inputs
