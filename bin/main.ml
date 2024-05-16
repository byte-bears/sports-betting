open Owl
open Sports_betting.Neural_network
open Sports_betting.Processing
open Sports_betting.Utils
open Sports_betting.Load
open Sports_betting.Ema

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

let mat = load_string_array "data/boxscores.csv"
let () = make_rectangular_cols mat ""

let stats =
  [
    ("Rebounds", "REB");
    ("Assists", "AST");
    ("Steals", "STL");
    ("Blocks", "BLK");
    ("Points", "PTS");
  ]

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

let apply_ema_to_data ema_type =
  match ema_type with
  | SimpleAverage -> simple_average_print ()
  | WeightedAverage -> weighted_average_print ()
  | SimpleMovingAverage -> simple_moving_average ()
  | WeightedMovingAverage -> weighted_moving_average ()

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
    | _ -> begin
        print_endline "";
        print_endline "Invalid response. Please choose a valid number.";
        select_ema ()
      end
  with Failure _ ->
    begin
      print_endline "";
      print_endline "Invalid input. Please enter a valid integer.";
      select_ema ()
    end

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
    | _ -> begin
        print_endline "";
        print_endline "Invalid response. Please choose a valid number.";
        select_algo ()
      end
  with Failure _ ->
    begin
      print_endline "";
      print_endline "Invalid input. Please enter a valid integer.";
      select_algo ()
    end

let rec print_stat_descriptions () =
  print_endline "";
  print_endline "These are the basketball statistics supported:";
  print_endline "";
  print_endline "1. Rebounds";
  print_endline "2. Assists";
  print_endline "3. Steals";
  print_endline "4. Blocks";
  print_endline "5. Points";
  print_endline "";
  print_endline "Which NBA statistic would you like metrics on?";
  let input = read_line () in
  try
    let num = int_of_string input in
    match num with
    | 1 -> "REB"
    | 2 -> "AST"
    | 3 -> "STL"
    | 4 -> "BLK"
    | 5 -> "PTS"
    | _ ->
        print_endline "";
        print_endline "Invalid response. Please choose a valid number.";
        print_stat_descriptions ()
  with Failure _ ->
    print_endline "";
    print_endline "Invalid input. Please enter a valid integer.";
    print_stat_descriptions ()

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

let check_in_list input list = List.mem input list

let display_nba_teams () =
  print_endline "Here is the list of NBA team names:";
  let abbr, teams = teams_list mat in
  print_list teams ~interp:"\n";
  teams_list mat

let rec isolate_nba_team the_input (abbr, teams) =
  match (abbr, teams) with
  | [], _ -> ("N/A", "N/A")
  | _, [] -> ("N/A", "N/A")
  | abbr :: _, team :: _
    when String.uppercase_ascii the_input = String.uppercase_ascii abbr -> begin
      (abbr, team)
    end
  | _ :: t1, _ :: t2 -> isolate_nba_team the_input (t1, t2)

let display_team_players team =
  print_endline "";
  print_string "Here is the list of players on ";
  print_string team;
  print_endline ":";
  print_list (player_list mat team) ~interp:"\n";
  player_list mat team

let rec request_for_player input teams_info =
  if check_in_list input (fst teams_info) then begin
    let team_info = isolate_nba_team input teams_info in
    let players = display_team_players (fst team_info) in
    print_string "Which player on ";
    print_string (snd team_info);
    print_endline " would you like statistics on?";
    let the_input = read_line () in
    if check_in_list the_input players then the_input
    else request_for_player input teams_info
  end
  else
    let teams_info = display_nba_teams () in
    print_endline "Which team would you like linear regression statistics for?";
    let the_input = read_line () in
    request_for_player the_input teams_info

let rec choose_ema () =
  print_endline "";
  print_endline "Please choose which exponential moving average to use";
  print_endline "1. Simple Average";
  print_endline "2. Weighted Average";
  print_endline "3. Simple Moving Average";
  print_endline "4. Weighted Moving Average";
  let input = read_line () in
  try
    let num = int_of_string input in
    match num with
    | 1 -> SimpleAverage (* TIM'S FUNCTIONS*)
    | 2 -> WeightedAverage
    | 3 -> SimpleMovingAverage
    | 4 -> WeightedMovingAverage
    | _ ->
        print_endline "";
        print_endline "Invalid response. Please choose a valid number.";
        choose_ema ()
  with Failure _ ->
    print_endline "";
    print_endline "Invalid input. Please enter a valid integer.";
    choose_ema ()

let choose_stat () =
  print_endline "";
  let stat = print_stat_descriptions () in
  stat

let rec retrieve_ema_info () =
  let teams_info = display_nba_teams () in
  print_endline "Which team would you like to perform an exponential moving";
  print_endline "average formula on?";
  let input = read_line () in
  if check_in_list input (fst teams_info) then begin
    let team_info = isolate_nba_team input teams_info in
    print_string "Which player on ";
    print_string (snd team_info);
    print_endline " would you like ";
    print_endline "statistics on?";
    let players = display_team_players (fst team_info) in
    let the_input = read_line () in
    if check_in_list the_input players then the_input else "N/A"
  end
  else retrieve_ema_info ()

let linear_regression_questionnaire () =
  let teams_info = display_nba_teams () in
  print_endline "Which team would you like linear regression statistics for?";
  let the_input = read_line () in
  let player = request_for_player the_input teams_info in
  let stat = choose_stat () in
  print_string stat;
  print_string player (* linear regression function *)

let rec visualize_ema_info () =
  print_endline "";
  print_endline "Would you like to learn about a specific exponential";
  print_endline "moving average formula? (Y/n)";
  let the_input = read_line () in
  match the_input with
  | "Y" -> begin
      let algo = select_ema () in
      describe_ema_algorithm algo;
      visualize_ema_info ()
    end
  | "n" ->
      let player = retrieve_ema_info () in
      let stat = choose_stat () in
      let ema_type = choose_ema () in
      apply_ema_to_data ema_type;
      ( (* let ema_type = choose_ema () in let player = retrieve_ema_info () in
           match ema_type with *) )
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
  | "Y" -> begin
      view_visualizations algo;
      learn_about_more_algos_or_visualize algo
    end
  | "n" -> begin
      let algo = select_algo () in
      describe_data_output algo;
      learn_about_more_algos_or_visualize algo
    end
  | "q" -> begin
      print_endline "";
      print_endline
        "Thank you for using the Sports Betting Algorithm Simulator!"
    end
  | _ -> learn_about_more_algos_or_visualize algo

(* Main interactive function *)
let main () =
  print_endline "Welcome to the Sports Betting Algorithm Simulator!";
  print_endline "";
  print_endline
    "This interactive tool is designed to help users explore and understand";
  print_endline
    "various statistical models and their applications in predicting outcomes";
  print_endline "in sports betting, specifically for player over/unders.";
  print_endline "";
  print_endline
    "You will have the opportunity to choose from three main types of data";
  print_endline
    "analysis algorithms: Neural Network, Linear Regression, and Exponential";
  print_endline
    "Moving Average. Each algorithm offers a unique approach to modeling sports";
  print_endline
    "data—Neural Networks analyze complex patterns in large datasets, Linear";
  print_endline
    "Regression examines direct correlations between simple variables, and";
  print_endline
    "Exponential Moving Averages focus on smoothing data to highlight trends by";
  print_endline "prioritizing recent performances.";
  print_endline "";
  print_endline
    "Additionally, you can delve deeper into Exponential Moving Averages with";
  print_endline
    "options such as Simple Average, Weighted Average, Simple Moving Average,";
  print_endline
    "and Weighted Moving Average, each providing different insights based on";
  print_endline "the weight and focus given to the data points.";
  print_endline
    "This simulator aims to enhance your understanding of how these algorithms";
  print_endline
    "can be applied to real-world betting scenarios, helping you make informed";
  print_endline "decisions based on statistical evidence.";
  extract_data ();
  let algo = select_algo () in
  describe_data_output algo;
  learn_about_more_algos_or_visualize algo

(* Start the main function *)
let () = main ()
