exception Out_of_bounds
(** [Out_of_bounds] is raised when an operation cannot get an element as either
    the value does not exist or is outside the allocated space of the column. *)

type t = {
  mutable data : string array;
  mutable size : int;
}
(** [t] represents the column *)

val empty : int -> t
(** [empty] is the value of an empty column with capacity [capacity] *)

val size : t -> int
(** [size] takes an input column [col] and outputs the number of elements inside
    it *)

val capacity : t -> int
(** [capacity] takes an input column [col] and outputs the total number of
    elements that the column can store *)

val add : t -> string -> unit
(** [add] takes a element [elt] and adds it into the an input column [col] at
    the rightmost empty index *)

val remove : t -> unit
(** [remove] takes an input column [col] and removes the element stored at the
    rightmost non-empty index. Raises [Out_of_bounds] if you remove from an
    empty column *)

val get : t -> int -> string
(** [get] takes an input column [col] and an index [idx] and returns the value
    stored in the column at that index. Raises [Out_of_bounds] if the specified
    index is less then 0 or greater then the column size *)

val make : string list -> t -> t
(** [make] takes an empty column [col] and a list of strings [lst] and makes an
    instance of a column that has all the elements of list inside it *)

val make_from_array : string array -> t -> t
(** [make_from_array] takes an empty column [col] and a array of strings [lst]
    and makes an instance of a column that has all the elements of array inside
    it *)

val fold_left : ('a -> string -> 'a) -> 'a -> t -> 'a
(** [fold_left] takes a function [f] a accumulator [x] and a column [col] and
    uses these to compute a final value based on the function and accumulator *)

val sub : t -> int -> int -> t
(** [sub] takes a column [col] a beginning index [pos] and a length [len] and
    uses that to create a new column whose elements are the subset of the
    original. The subset will be the elements from pos to pos + len - 1 *)

val extend : t -> int -> t
(** [extend] takes a column [col] and a new length [new_size] and fills the
    column with empty strings until we get to a column with capacity new_size *)

val to_string : int -> t -> string
(** [to_string] takes a column [col] and a integer [n] and converts the col it
    into a string which each segment of the column being [n] ASCII characters
    long *)

val max_length : t -> int
(** [max_length] takes a column [col] finds the longest element within that
    column and returns the length of that element *)

val to_float_column : t -> float array
(** [to_float_column] takes a column [col] and converts it into a float array *)

val max_length_arr : string array -> int
(** [max_length_arr] takes a string array [arr] finds the string element with
    the maximum length *)
