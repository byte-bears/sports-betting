val team_encoding : string -> string
(** [team_encoding team] is a function that takes a [team] and returns the
    encoded version of the team's name. The team name is encoded as a string
    representation of a float. *)

val add_matchup_stats : string array array -> string array array
(** [add_matchup_stats data] is a function that takes a string matrix [data] and
    returns a modified version of the matrix with additional matchup statistics.
    The function adds matchup statistics for each game, adding columns for
    whether the game was a home game "HOME" and the encoding of the opponent
    "OPP" *)

val get_player_data : string array array -> string -> string array array
(** [get_player_data data player] is a function that takes a string matrix
    [data] and a [player] and returns a modified version of the array containing
    only the data for the specified player. *)

val get_player_stats :
  string array array -> string -> string array -> string array array
(** [get_player_stats data player stats] is a function that takes a string
    matrix [data], a [player], and an array of types of stats [stats] and
    returns a modified version of the array containing only the specified
    player's stats specified in [stats]. *)

val get_player_stat : string array array -> string -> string -> float array
(** [get_player_stat data player stat] is a function that takes a string matrix
    [data], a [player], and a [stat] and returns an array of floats representing
    the specified player's statistics for the specified [stat]. *)

val period_data :
  (string array array -> string -> string -> float array) ->
  string array array ->
  string ->
  string ->
  int ->
  float array array * float array
(** [period_data data_f data player stat period] is a function that takes a
    function [data_f], three arguments [data], [player], [stat], and an integer
    [period]. It returns a tuple containing two arrays: a matrix and a vector.
    First, [data_f] is applied to extract data based on the [player] and [stat].
    Then, the matrix is constructed from restructuring that stat into a feature
    vector of the past [period] stats while the current value of the [stat] is
    added to the vector. *)

val interpolated_data :
  string array array -> string -> string array -> int -> string array array
(** [interpolated_data data player stats period] is a function that takes a
    string matrix [data], a [player], an array [stats], and an integer [period].
    It returns a modified version of the array containing interpolated data for
    the specified player's statistics for the specified stat names, taking data
    every [period] days. *)

val stack : 'a array array -> 'a array array -> 'a array array
(** [stack data1 data2] is a function that takes two matrices [mat1] and [mat2]
    and returns a new matrix that is the result of stacking each column of
    [mat1] on top of each column of [mat2], basically serving to combine
    features. *)

val teams_list : string array array -> string list * string list
(** [teams_list data] is a function that takes a string matrix [data] and
    returns a tuple containing two lists: a list of team abbreviations and a
    list of team names + abbreviations combined into a nice printable style. *)

val player_list : string array array -> string -> string list
(** [player_list data team] is a function that takes a string matrix [data] and
    a [team] and returns a list of player names for the specified [team]. *)

val good_features :
  string array array ->
  string ->
  ?period:int ->
  string ->
  Datatable.t * Column.t
(** [good_features data player ?period stat] is a function that takes a string
    matrix [data], a [player], an optional [period], and a [stat]. It returns a
    tuple containing a datatable and a column. The datatable contains the good
    features, created by using [stack] on [period_data] and [interpolated_data]
    with the given [period], combining past stats with additional stats. The
    column contains the labels aka the current value of the [stat] *)
