val load_string_array : string -> string array array
(** [load_string_array filename] loads a string matrix from the given
    [filename], reading the contents of the file as a csv. *)

val load_float_col : string array -> float array
(** [load_float_col arr] converts a string array [arr] into a float array. It
    parses each element of the array as a float, discarding elements that cannot
    be converted. *)

val load_float_array : string array array -> float array array
(** [load_float_array arr] converts a string matrix [arr] into a float matrix.
    It calls [load_float_col] on each column in the matrix. *)

val filter_cols : 'a array array -> 'a array -> 'a array array
(** [filter_cols mat cols] filters the columns of a matrix [mat] down to just
    the given [cols]. *)

val filter_by_col : string array array -> string -> string -> string array array
(** [filter_by_col mat col value] filters the rows of a string matrix [mat]
    based on the given [col] and [value], keeping the header plus rows where the
    value of [col] matches [value]. *)

val get_col : 'a array array -> 'a -> 'a array
(** [get_col mat col] extracts [col] from a matrix [mat], returning an empty
    array [[||]] if no column matches. *)

val binarize_col : ('a -> 'b) -> 'a array -> 'b array
(** [binarize_col f arr] applies the function [f] to each element of an array
    [arr]. *)

val add_col : 'a array array -> 'a array -> 'a array array
(** [add_col mat col] adds a new column [col] to a matrix [mat]. *)

val is_rectangular : 'a array array -> bool
(** [is_rectangular mat] checks if a matrix [mat] is rectangular, i.e., all rows
    have the same length. *)

val max_length : 'a array array -> int
(** [max_length mat] returns the maximum length of any column in a matrix [mat]. *)

val make_rectangular_rows : float array array -> float array array
(** [make_rectangular_rows mat] makes a matrix [mat] rectangular by padding rows
    with zeros. *)

val make_rectangular_cols : 'a array array -> 'a -> unit
(** [make_rectangular_cols mat fill] makes a matrix [mat] rectangular by padding
    columns with a specified value [fill]. *)
