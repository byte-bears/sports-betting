open Owl_base
open Dt

let data_names =
  [
    "SEASON_ID";
    "PLAYER_ID";
    "PLAYER_NAME";
    "TEAM_ID";
    "TEAM_ABBREVIATION";
    "TEAM_NAME";
    "GAME_ID";
    "GAME_DATE";
    "MATCHUP";
    "WL";
    "MIN";
    "FGM";
    "FGA";
    "FG_PCT";
    "FG3M";
    "FG3A";
    "FG3_PCT";
    "FTM";
    "FTA";
    "FT_PCT";
    "OREB";
    "DREB";
    "REB";
    "AST";
    "STL";
    "BLK";
    "TOV";
    "PF";
    "PTS";
    "PLUS_MINUS";
    "FANTASY_PTS";
    "VIDEO_AVAILABLE";
  ]

let data =
  Dt.make "../data/boxscores.csv" data_names
    (Dt.empty (List.length data_names) 0)
