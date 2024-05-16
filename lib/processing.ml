open Batteries

let team_encoding_east team =
  match team with
  | "BOS" -> "0"
  | "BKN" -> "1"
  | "NYK" -> "2"
  | "PHI" -> "3"
  | "TOR" -> "4"
  | "CHI" -> "5"
  | "CLE" -> "6"
  | "DET" -> "7"
  | "IND" -> "8"
  | "MIL" -> "9"
  | "ATL" -> "10"
  | "CHA" -> "11"
  | "MIA" -> "12"
  | "ORL" -> "13"
  | "WAS" -> "14"
  | team -> failwith ("This team (" ^ team ^ ") does not exist")

let team_encoding team =
  match team with
  | "OPP" -> "OPP"
  | "DEN" -> "0"
  | "MIN" -> "1"
  | "OKC" -> "2"
  | "POR" -> "3"
  | "UTA" -> "4"
  | "GSW" -> "5"
  | "LAC" -> "6"
  | "LAL" -> "7"
  | "PHX" -> "8"
  | "SAC" -> "9"
  | "DAL" -> "10"
  | "HOU" -> "11"
  | "MEM" -> "12"
  | "NOP" -> "13"
  | "SAS" -> "14"
  | team -> team_encoding_east team

let matchup_helper size matchups teams opp home =
  for i = 1 to size - 1 do
    let matchup = matchups.(i) in
    let team = teams.(i) in
    let split = Str.split (Str.regexp " ") matchup in
    let team1 = List.nth split 0 in
    let symb = List.nth split 1 in
    let team2 = List.nth split 2 in
    let team1 = Utils.strip_str team1 in
    let symb = Utils.strip_str symb in
    let team2 = Utils.strip_str team2 in
    if team1 = team then opp.(i) <- team2 else opp.(i) <- team1;
    if symb = "@" then home.(i) <- "0" else home.(i) <- "1"
  done

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
  let () = matchup_helper size matchups teams opp home in
  let opp = Array.map team_encoding opp in
  let data = Load.add_col data opp in
  let data = Load.add_col data home in
  data

let get_player_data data player =
  let data = Load.filter_by_col data "PLAYER_NAME" player in
  let data = add_matchup_stats data in
  data

let get_player_stats data player stats =
  let data = Load.filter_by_col data "PLAYER_NAME" player in
  let features = Array.append [| "PLAYER_NAME" |] stats in
  let data = add_matchup_stats data in
  let data = Load.filter_cols data features in
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
  let max_loop = (Array.length full_data / period) - 1 in
  for i = 0 to max_loop do
    let ind = i * period in
    let data_point = Array.sub full_data ind period in
    let label = full_data.(ind + period) in
    data := data_point :: !data;
    labels := label :: !labels
  done;
  let data = Array.of_list (List.rev !data) in
  let labels = Array.of_list (List.rev !labels) in
  (data, labels)

let interpolated_data data player stats period =
  let cols = Array.append [| "PLAYER_NAME" |] stats in
  let data = Load.filter_cols data cols in
  let data = Load.filter_by_col data "PLAYER_NAME" player in
  let out = ref [] in
  let max_loop = (Array.length data / period) - 1 in
  for i = 0 to max_loop do
    let ind = ((i + 1) * period) - 1 in
    let data_point = Array.sub data ind period in
    out := data_point :: !out
  done;
  !out

(* let vstack *)

let teams_list data =
  let data = Load.filter_cols data [| "TEAM_ABBREVIATION"; "TEAM_NAME" |] in
  let visited = ref [] in
  let ret_abb = ref [] in
  let ret = ref [] in
  for i = 1 to Array.length data.(0) - 1 do
    let team = data.(1).(i) in
    if not (List.mem team !visited) then (
      visited := team :: !visited;
      ret_abb := data.(0).(i) :: !ret_abb;
      ret := Printf.sprintf "%s (%s)" data.(1).(i) data.(0).(i) :: !ret)
  done;
  (!ret_abb, !ret)

let player_list data team =
  let data = Load.filter_cols data [| "PLAYER_NAME"; "TEAM_ABBREVIATION" |] in
  let data = Load.filter_by_col data "TEAM_ABBREVIATION" team in
  let players = Load.get_col data "PLAYER_NAME" in
  let players = Array.to_list players in
  let players = List.unique players in
  match players with
  | [] -> failwith "Team column doesn't exist, cannot make new data"
  | h :: t -> t
