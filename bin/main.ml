open Owl
open Sports_betting.Neural_network
open Sports_betting.Processing
open Sports_betting.Utils
open Sports_betting.Load

(* Define the types of algorithms *)
type dataOutput =
  | NeuralNetwork
  | LinearRegression
  | ExponentialMovingAverage

type exponentialMovingAve =
  | SimpleAverage
  | WeightedAverage
  | SimpleMovingAverage
  | WeightedMovingAverage

(* type goodStat = | REB | AST | STL | BLK | PTS rebounds, assists, steals,
   blocks, points *)

let mat = load_string_array "data/boxscores.csv"
let () = make_rectangular_cols mat ""

let simple_average_print () =
  print_endline "";
  print_endline
    "Simple Average: Calculates the average performance (e.g., points,";
  print_endline
    "rebounds, assists) of a basketball player over a set period. This";
  print_endline
    "method is straightforward and effective for players with consistent";
  print_endline "performance but may not account for recent form or opposition";
  print_endline "strength."

let weighted_average_print () =
  print_endline "";
  print_endline
    "Weighted Average: Similar to a simple average but assigns weights to";
  print_endline
    "games based on recentness or the strength of the opposition. For";
  print_endline
    "example, performances against stronger opponents or more recent games";
  print_endline
    "might be given more importance, providing a more nuanced prediction";
  print_endline "for player over/unders."

let simple_moving_average () =
  print_endline "";
  print_endline
    "Simple Moving Average: Calculates the average of a player’s stats over";
  print_endline
    "a specific number of recent games, sliding the window forward as new";
  print_endline
    "games occur. This method smooths out anomalies and provides a clearer";
  print_endline
    "picture of a player’s current form, useful for trend analysis in";
  print_endline "volatile performance periods."

let weighted_moving_average () =
  print_endline "";
  print_endline
    "Weighted Moving Average: Improves upon the simple moving average by";
  print_endline
    "applying more weight to recent games, under the assumption that the";
  print_endline
    "latest performances are more indicative of future results. This can be";
  print_endline
    "particularly effective in adjusting for mid-season changes, such as";
  print_endline "player injuries or tactical shifts."

let describe_neural_network () =
  print_endline "";
  print_endline "Neural Network: Employs deep learning models to predict player";
  print_endline
    "performances by learning from complex patterns in large datasets. This";
  print_endline
    "algorithm can take into account various factors like player age,";
  print_endline "career stage, physical condition, and even social media";
  print_endline "sentiment to forecast game-specific over/unders more";
  print_endline "accurately."

let describe_linear_regression () =
  print_endline "";
  print_endline "The Linear Regression model provides predictions for ";
  print_endline "player over/unders using a simple linear approach to ";
  print_endline "estimate future performances based on past data. It ";
  print_endline "identifies correlations between variables such as minutes";
  print_endline "played and points scored to forecast outcomes. This model";
  print_endline "articularly useful for stable players with consistent ";
  print_endline "performance trends over time."

let describe_exponential_moving_average () =
  print_endline "";
  print_endline "The Exponential Moving Average model uses smoothing ";
  print_endline "techniques to forecast player over/unders by prioritizing";
  print_endline "more recent performances. It adjusts more quickly to ";
  print_endline "changes in player form and external conditions than ";
  print_endline "simple moving averages. This model is ideal for ";
  print_endline "dynamically assessing players whose performance might be";
  print_endline "influenced by recent physical conditions or team";
  print_endline "strategies."

(* Function to print descriptions *)
let describe_ema_algorithm ema_type =
  match ema_type with
  | SimpleAverage -> simple_average_print ()
  | WeightedAverage -> weighted_average_print ()
  | SimpleMovingAverage -> simple_moving_average ()
  | WeightedMovingAverage -> weighted_moving_average ()

let describe_data_output data_output =
  match data_output with
  | NeuralNetwork -> describe_neural_network ()
  | LinearRegression -> describe_linear_regression ()
  | ExponentialMovingAverage -> describe_exponential_moving_average ()

(* Function to simulate data extraction *)
let extract_data () =
  print_endline "";
  print_endline "Simulating data extraction from the internet...";
  (* In a real scenario, here you would fetch and process real data *)
  print_endline "Data extracted successfully!"

(* Interactive selection of algorithms *)
let rec select_ema () =
  print_endline "";
  print_endline "Please choose which exponential moving average to learn";
  print_endline "about:";
  print_endline "1. Simple Average";
  print_endline "2. Weighted Average";
  print_endline "3. Simple Moving Average";
  print_endline "4. Weighted Moving Average";
  let input = read_line () in
  try
    let num = int_of_string input in
    match num with
    | 1 -> SimpleAverage
    | 2 -> WeightedAverage
    | 3 -> SimpleMovingAverage
    | 4 -> WeightedMovingAverage
    | _ ->
        print_endline "";
        print_endline "Invalid response. Please choose a valid number.";
        select_ema ()
  with Failure _ ->
    print_endline "";
    print_endline "Invalid input. Please enter a valid integer.";
    select_ema ()

let rec select_algo () =
  print_endline "";
  print_endline "Please choose an algorithm to learn more:";
  print_endline "1. Neural Network";
  print_endline "2. Linear Regression";
  print_endline "3. Exponential Moving Average";
  let input = read_line () in
  try
    let num = int_of_string input in
    match num with
    | 1 -> NeuralNetwork
    | 2 -> LinearRegression
    | 3 -> ExponentialMovingAverage
    | _ ->
        print_endline "";
        print_endline "Invalid response. Please choose a valid number.";
        select_algo ()
  with Failure _ ->
    print_endline "";
    print_endline "Invalid input. Please enter a valid integer.";
    select_algo ()

let visualize_neural_network () =
  let layers = [ Dense (5, ReLU); Dense (5, Sigmoid) ] in
  let input_dim = 5 in
  let network = create layers input_dim in
  let inputs = List.init 10 (fun _ -> Mat.uniform 1 5) in
  let actuals = List.init 10 (fun _ -> Mat.uniform 1 5) in

  let epochs = 1000 in
  let learning_rate = 0.01 in
  let trained_network = train network inputs actuals epochs learning_rate in

  List.iter
    (fun input ->
      let output = forward trained_network input in
      Printf.printf "Predicted Output:\n";
      List.iter Mat.print output)
    inputs

let display_nba_teams () =
  print_endline "";
  print_endline "Here is the list of NBA team names:";
  print_list (teams_list mat) ~interp:"\n"

let linear_regression_questionnaire () =
  display_nba_teams ();
  print_endline "Which team would you like linear regression statistics for?";
  let the_input = read_line () in
  match the_input with
  | "Y" -> print_endline ""
  | _ -> print_endline ""

let rec visualize_ema_info () =
  print_endline "";
  print_endline "Would you like to learn about a specific exponential";
  print_endline "moving average formula? (Y/n)";
  let the_input = read_line () in
  match the_input with
  | "Y" ->
      let algo = select_ema () in
      describe_ema_algorithm algo;
      print_endline "W"
  | "n" ->
      print_endline "Which team would you like to perform an exponential moving";
      print_endline "average formula on?";
      display_nba_teams ()
  | _ -> visualize_ema_info ()

let view_visualizations algo =
  match algo with
  | NeuralNetwork -> visualize_neural_network ()
  | LinearRegression -> linear_regression_questionnaire ()
  | ExponentialMovingAverage -> visualize_ema_info ()

let rec learn_about_more_algos_or_visualize algo =
  print_endline "";
  print_endline "Would you like to visualize the data output, learn about our";
  print_endline "other data outputs, or quit? (Y/n/q)";
  let the_input = read_line () in
  match the_input with
  | "Y" ->
      view_visualizations algo;
      learn_about_more_algos_or_visualize algo
  | "n" ->
      let algo = select_algo () in
      describe_data_output algo;
      learn_about_more_algos_or_visualize algo
  | "q" ->
      print_endline
        "Thank you for using the Sports Betting Algorithm Simulator!"
  | _ -> learn_about_more_algos_or_visualize algo

(* Main interactive function *)
let main () =
  print_endline "Welcome to the Sports Betting Algorithm Simulator!";
  extract_data ();
  let algo = select_algo () in
  describe_data_output algo;
  learn_about_more_algos_or_visualize algo

(* Start the main function *)
let () = main ()
