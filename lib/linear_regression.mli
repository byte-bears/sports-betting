val theta : float array array -> float array -> Torch.Tensor.t
(** [theta x y] computes the theta values for linear regression using the input
    feature matrix [x] and the target values [y], return a tensor containing the
    values. *)

val theta_datatable : Datatable.t -> Column.t -> Torch.Tensor.t
(** [theta_datatable dt col] computes the theta values for linear regression
    using the input datatable [dt] and the target column [col], as a tensor
    containing the values. *)
