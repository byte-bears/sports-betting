val load_string_array : string -> string array array
val load_float_col : string array -> float array
val load_float_array : string array array -> float array array
val filter_cols : 'a array array -> 'a array -> 'a array array
val get_col : 'a array array -> 'a -> 'a array
