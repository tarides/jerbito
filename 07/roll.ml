type 'a t = 'a * 'a list

let empty = []

let unit x = (x, empty)
let cons x (y, u) = x, y :: u
let hd (x, _) = x
let tl (_, u) = match u with
  | [] -> failwith "tl"
  | x :: u -> (x, u);;

(* pour ajouter les fonction suivantes: *)
(* (* length : 'a t -> int *)
let rec length (x, u) =
  match u with
  | [] -> if x = None then 0 else 1
  | _ :: u' -> 1 + length (x, u');;
(* append : 'a t -> 'a t -> 'a t *)
let rec append list1 list2 =
  match list1 with
  | [] -> list2
  | hd :: tl -> hd :: append tl list2 *)

(* let length u =
  List.length (snd u)

let append u1 u2 =
  let (_, l1) = u1 in
  let (_, l2) = u2 in
  (hd u1, l1 @ l2)

(* let concat uu =
  List.fold_left append empty (snd uu) *)

let rev u =
  let (_, l) = u in
  (hd u, List.rev l)

let map f u =
  let (_, l) = u in
  (f (hd u), List.map f l)

(* let concat_map f u =
  let (_, l) = u in
  let mapped = List.map f l in
  List.fold_left append empty mapped *)
let filter f u =
  let (_, l) = u in
  (hd u, List.filter f l) *)