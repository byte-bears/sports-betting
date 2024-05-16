val strip_str : string -> string
(** [strip_str str] is a function that removes leading and trailing whitespace
    from a string [str]. *)

val print_list : ?interp:string -> string list -> unit
(** [print_list ?interp lst] is a function that prints the elements of a string
    list [lst] to the console. It accepts an optional [interp] parameter which
    specifies the separator between elements, defaulting to space if nothing is
    provided. *)

val float_to_string_arr : float array -> string array
(** [float_to_string_arr arr] is a function that converts a float array [arr] to
    a string array. *)

val print_arr : ?interp:string -> string array -> unit
(** [print_arr ?interp arr] is a function that prints the elements of a string
    array [arr] to the console. It accepts an optional [interp] parameter which
    specifies the separator between elements, defaulting to space if nothing is
    provided. *)

val print_col_lengths : string array array -> unit
(** [print_col_lengths mat] is a function that prints the lengths of each column
    in a string matrix [mat], mainly used to debug non-rectangular matrices. *)

val print_length : 'a array -> unit
(** [print_length arr] is a function that prints the length of an array [arr]. *)

val shape : 'a array array -> int * int
(** [shape mat] is a function that returns the shape of a matrix [mat] as a
    tuple of two integers. *)

val print_shape : 'a array array -> unit
(** [print_shape mat] is a function that prints the shape of a matrix [mat]. *)

val float_to_string_mat : float array array -> string array array
(** [float_to_string_mat mat] is a function that converts a float matrix [mat]
    to a string matrix. *)

val print_mat : string array array -> unit
(** [print_mat mat] is a function that prints the elements of a string matrix
    [mat]. *)

val print_mat_colwise : string array array -> unit
(** [print_mat_colwise mat] is a function that prints the elements of a string
    matrix [mat] column-wise to the console. *)

val transpose : string array array -> string array array
(** [transpose mat] is a function that transposes a string matrix [mat]. *)
