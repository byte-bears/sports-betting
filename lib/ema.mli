val simple_average : Column.t -> float
(** [simple_average col] calculates the simple average of the values in the
    given column [col]. *)

val simple_average_list : int list -> float
(** [simple_average_list lst] calculates the simple average of the values in the
    given integer list [lst]. *)

val weighted_average : Column.t -> float -> float
(** [weighted_average col weight] calculates the weighted average of the values
    in the given column [col], using the specified weight [weight]. *)

val weighted_average_list : float list -> float -> float
(** [weighted_average_list lst weight] calculates the weighted average of the
    values in the given float list [lst], using the specified weight [weight]. *)

val simple_moving_average : Column.t -> float list
(** [simple_moving_average col] calculates the simple moving average of the
    values in the given column [col], as a list of values. *)

val weighted_moving_average : float -> Column.t -> float list
(** [weighted_moving_average weight col] calculates the weighted moving average
    of the values in the given column [col], using the specified weight
    [weight], as a list of values. *)

val simple_moving_average_list : float list -> float list
(**[simple_moving_average_list lst] calculates the simple moving average of the
   values in the given float list [lst], as a list of values. *)

val weighted_moving_average_list : float -> float list -> float list
(**[weighted_moving_average_list weight lst] calculates the weighted moving
   average of the values in the given float list [lst], using the specified
   weight [weight], as a list of values. *)
