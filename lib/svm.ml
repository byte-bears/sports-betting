module Np = Np.Numpy

(* import csv into 2d numpy array *)
(* let csv2nparray file = let data = Csv.load file in let data = List.map (fun
   row -> Array.of_list row) data in let data = Array.of_list data in
   Np.asmatrix data *)

let hi file = Np.loadtxt ~delimiter:"," ~skiprows:1 ~fname:(`S file) ()

(* split data into training and test sets *)
