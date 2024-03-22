(* Exercice I : Suites numériques*)

let rec u (n:int) (c:float) (t:float) (m:float) : float =
if (n=0) then c
else ((1.+.t)*.(u (n-1) c t m));

(* Exercice II : Distance de Hamming *)

(* Q1 *)
let rec nbtrue (bs:bool list) : int =
match bs with
[] -> 0
| true::bs -> 1+(nbtrue bs)
| _::bs -> (nbtrue bs);;

(* recursif terminal *)
let nbtrue_rt (bs:bool list) : int =
let rec loop bs r =
match bs with
[] -> r
| true::bs -> 1+(nbtrue bs)
| _::bs -> (nbtrue bs)
in
(loop bs 0)
(* fold_left *)
let nbtrue_it (bs:bool list) : int =
List.fold_left (fun r b -> if b then 1+r else r) 0 bs
(* Q2 *)
let rec sumb (xs:’a list) (ys:’a list) : bool list =
match xs, ys with
[], [] -> []
| x::xs, y::ys -> (x<>y)::(sumb xs ys)
| _ -> raise (Invalid_argument "sumb")
(* Q3 *)
let disth (xs:’a list) (ys:’a list) : int =
(nbtrue (sumb xs ys))

(* Exercice III : Tri rapide *)
(* Q1 *)
let rec list_le (x:int) (xs:int list) : int list =
match xs with
[] -> []
| y::xs -> if (y <= x) then y::(list_le x xs)
else (list_le x xs)
let rec list_gt (x:int) (xs:int list) : int list =
match xs with
[] -> []
| y::xs -> if (y > x)then y::(list_gt x xs)
else (list_gt x xs)
(* filter *)
let list_le (x:int) (xs:int list) : int list =
let le_x y = y <= x in
List.filter le_x xs
let list_gt (x:int) (xs:int list) : int list =
let gt_x y = y > x in
List.filter gt_x xs
(* Q2 *)
let rec qsort (xs:int list) : int list =
match xs with
[] -> []
| x::[] -> xs
| x::xs -> (qsort (list_le x xs)) @ [x] @ (qsort (list_gt x xs))

(* Exercice IV : Différences dans une liste*)
let rec all_diff (xs:’a list) : bool =
match xs with
[] -> true
| x::xs -> (not (List.mem x xs)) && (all_diff xs)
let rec nb_diff (xs:’a list) : int =
match xs with
[] -> 0
| x::xs -> if (List.mem x xs) then (nb_diff xs)
else 1+(nb_diff xs)
let rec at_least_n_diff (n:int) (xs:’a list) : bool =
if (n=0) then true
else match xs with
[] -> false
| x::xs -> if (List.mem x xs) then (at_least_n_diff n xs)
else (at_least_n_diff (n-1) xs)
let _ = assert ((at_least_n_diff (-1) [7;8;7;0;5;4;5])==false)
