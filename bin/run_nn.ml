open Owl
open Sports_betting.Neural_network

let () =
  (* Define the network architecture *)
  let layers = [ Dense (5, ReLU); Dense (5, Sigmoid) ] in
  let input_dim = 5 in
  let network = create layers input_dim in
  let inputs = List.init 10 (fun _ -> Mat.uniform 1 5) in
  let actuals = List.init 10 (fun _ -> Mat.uniform 1 5) in

  List.iter (fun input -> Mat.print input) inputs;
  List.iter (fun actual -> Mat.print actual) actuals;
  let epochs = 1000 in
  let learning_rate = 0.01 in
  let trained_network = train network inputs actuals epochs learning_rate in

  List.iter
    (fun input ->
      let output = forward trained_network input in
      Printf.printf "Predicted Output:\n";
      List.iter Mat.print output)
    inputs
