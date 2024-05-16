open Batteries

let team_encoding team =
  match team with
  | "ATL" -> "0"
  | "BOS" -> "1"
  | "BKN" -> "2"
  | "CHA" -> "3"
  | "CHI" -> "4"
  | "CLE" -> "5"
  | "DAL" -> "6"
  | "DEN" -> "7"
  | "DET" -> "8"
  | "GSW" -> "9"
  | "HOU" -> "10"
  | "IND" -> "11"
  | "LAC" -> "12"
  | "LAL" -> "13"
  | "MEM" -> "14"
  | "MIA" -> "15"
  | "MIL" -> "16"
  | "MIN" -> "17"
  | "NOP" -> "18"
  | "NYK" -> "19"
  | "OKC" -> "20"
  | "ORL" -> "21"
  | "PHI" -> "22"
  | "PHX" -> "23"
  | "POR" -> "24"
  | "SAC" -> "25"
  | "SAS" -> "26"
  | "TOR" -> "27"
  | "UTA" -> "28"
  | "WAS" -> "29"
  | "OPP" -> "OPP"
  | team -> failwith ("This team (" ^ team ^ ") does not exist")

let add_matchup_stats data =
  let size = Array.length data.(0) in
  let rect = Load.is_rectangular data in
  (if not rect then
     let () = Utils.print_mat data in
     failwith "Data is not rectangular");
  let opp = Array.make size "" in
  let home = Array.make size "" in
  opp.(0) <- "OPP";
  home.(0) <- "HOME";
  (* get MATCHUP column, throw error if it doesn't exist *)
  let matchups = Load.get_col data "MATCHUP" in
  let teams = Load.get_col data "TEAM_ABBREVIATION" in
  if matchups = [||] || teams = [||] then
    failwith "Matchup or team column doesn't exist, cannot make new data";

  for i = 1 to size - 1 do
    let matchup = matchups.(i) in
    let team = teams.(i) in
    let matchup = Str.global_replace (Str.regexp "vs.") "@" matchup in
    let split = Str.split (Str.regexp "@") matchup in
    let team1 = List.nth split 0 in
    let team2 = List.nth split 1 in
    let team1 = Utils.strip_str team1 in
    let team2 = Utils.strip_str team2 in
    if team1 = team then (
      opp.(i) <- team2;
      home.(i) <- "0")
    else (
      opp.(i) <- team1;
      home.(i) <- "1")
  done;
  let opp = Array.map team_encoding opp in
  let data = Load.add_col data opp in
  let data = Load.add_col data home in
  data

let get_player_stat data player stat =
  let features = [| "PLAYER_NAME"; stat |] in
  let data = Load.filter_cols data features in
  let data = Load.filter_by_col data "PLAYER_NAME" player in
  let data = Load.get_col data stat in
  let data = Array.sub data 1 (Array.length data - 1) in
  Array.map float_of_string data

(** Create data to have past [period] stat points as data and the current stat
    as the label *)
let period_data data_f data player stat period =
  let full_data = data_f data player stat in
  let data = ref [] in
  let labels = ref [] in
  for i = 0 to Array.length full_data - period - 1 do
    let data_point = Array.sub full_data i period in
    let label = full_data.(i + period) in
    data := data_point :: !data;
    labels := label :: !labels
  done;
  let data = Array.of_list (List.rev !data) in
  let labels = Array.of_list (List.rev !labels) in
  (data, labels)
