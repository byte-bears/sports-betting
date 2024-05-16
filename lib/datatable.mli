exception Uneven_Table
(** [Uneven_Table] is raised when an operation causes the datatable to break the
    RI - columns in the datatable must have the same capacity *)

exception Out_of_bounds
(** [Out_of_bounds] is raised when an operation cannot get an element as either
    the value does not exist or is outside the allocated space of the datatable. *)

type t = {
  mutable headers : string array;
  mutable dt : Column.t array;
  mutable size : int;
}
(** [t] represents the datatable *)

val col_num : t -> int
(** [col_num] takes a input [table] and returns the number of columns in it *)

val row_num : t -> int
(** [row_num] takes a input [table] and returns the number of rows in it *)

val empty : int -> int -> t
(** [empty] is the value of an empty datatable with column size [col] and row
    size [row] *)

val add : t -> string -> Column.t -> unit
(** [add] takes a column [col], a table [table], and a name [name] and adds the
    column to the table at the first empty column value and the header into
    headers. If [col] size is too small or too large will expand or compress the
    col to the right size before adding to [table] *)

val max_length : t -> int
(** [max_length] takes a datatable [table] and finds the Column with the largest
    size within the datatable *)

val to_string : t -> string
(** [max_len] takes a datatable [table] and converts it into a string *)

val to_float_array : t -> float array array
(** [to_float_array] takes a datatable [table] and converts it into a float
    array array *)

val make : string array array -> string array -> t
(** [make] takes a string array array of data [matrix] and a string array of
    headers [names] and creates an instance of a datatable with [matrix] as its
    data and [names] as its header *)
