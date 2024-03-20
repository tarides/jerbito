(* Définition du type de file *)
type 'a queue = 'a list * 'a list

(* Fonction of_list : convertit une liste en une file *)
let of_list (lst : 'a list) : 'a queue =
  match lst with
  | [] -> failwith "of_list: queue cannot be empty"
  | x :: u -> (lst, u)

(* Fonction to_list : convertit une file en une liste *)
let to_list (q : 'a queue) : 'a list =
  let front, back = q in
  front @ List.rev back

(* Fonction snoc : ajoute un élément à la fin de la file *)
let snoc (x : 'a) ((front, back) : 'a queue) : 'a queue =
  (front, x :: back)

(* Fonction hd : retourne le premier élément de la file *)
let hd ((front, _) : 'a queue) : 'a =
  match front with
  | [] -> failwith "hd: queue is empty"
  | x :: _ -> x

(* Fonction rev : renverse une file *)
let rev (q : 'a queue) : 'a queue =
  let front, back = q in
  (back, front)









(********** Foncton D'ASSOCIATION : **********)

(* On definit d'abord la fonction def *) 
let def (f : 'a -> 'b) (x : 'a) (y : 'b) (z : 'a) : 'b =
  if x = z then
    y
  else
    f z;;

(* Définition de la fonction sq *)

let sq (x : int) : int =
let f = def (fun _ _ -> failwith "undefined") in
if x <= 10 then
  def (fun y _ -> y * y) x (f x) x
else
  failwith "undefined";;

(* Définition de la fonction cube *)


(* Définition de la fonction bogus_sq *)
let bogus_sq (x : int) : int =
  let f = def (fun _ -> x * x) in
  def (fun y -> x * x) x (f x) x

(* Définition de la fonction sq' utilisant List.assoc *)
let sq' (x : int) : int =
  let squares = List.map (fun x -> (x, x * x)) (List.init 11 (fun x -> x)) in
  try
    List.assoc x squares
  with
  | Not_found -> failwith "undefined"

(* Définition de la fonction cube' utilisant List.assoc *)

(* Fonction append : concatène deux files *)
let append (q1 : 'a queue) (q2 : 'a queue) : 'a queue =
  let front1, back1 = q1 in
  let front2, back2 = q2 in
  (front1 @ List.rev back1 @ front2, back2)

(* Fonction concat : concatène une file de files en une seule file *)
let concat (qq : 'a queue queue) : 'a queue =
  List.fold_left append ([], []) qq

(* Fonction filter : filtre les éléments d'une file en fonction d'un prédicat *)
let filter (p : 'a -> bool) ((front, back) : 'a queue) : 'a queue =
  let filtered_front = List.filter p front in
  let filtered_back = List.filter p back in
  (filtered_front, filtered_back)

(* Fonction map : applique une fonction à chaque élément d'une file *)
let map (f : 'a -> 'b) ((front, back) : 'a queue) : 'b queue =
  (List.map f front, List.map f back)

(* Fonction fold : applique une fonction d'accumulation à chaque élément d'une file *)
let fold (f : 'b -> 'a -> 'b) (acc : 'b) ((front, back) : 'a queue) : 'b =
  let acc' = List.fold_left f acc front in
  List.fold_left f acc' (List.rev back)












  (* Type pour les observables *)
  type 'a obs = 'a list;;

  (* Fonction obs_concat : concatène deux observables avec un décalage approprié *)
  let obs_concat (u : 'a obs) (v : 'a obs) : 'a obs =
    match u, v with
    | [], _ -> v
    | _, [] -> u
    | _ ->
        let len_u = List.length u in
        let delay = List.nth u (len_u - 1) in
        List.map (fun x -> x + delay) v;;

  (* Fonction obs_concat_list : concatène une liste d'observables *)
  let obs_concat_list (lists : 'a obs list) : 'a obs =
    List.fold_left obs_concat [] lists;; 

  (* Fonction obs_concat_map : applique une fonction à chaque élément d'un observable et concatène les résultats *)
  let obs_concat_map (f : 'a -> 'b obs) (obs : 'a obs) : 'b obs =
    obs_concat_list (List.map f obs);;





(* Fonction obs_concat : concatène deux observables avec un décalage approprié *)
let obs_concat (u : 'a obs) (v : 'a obs) : 'a obs =
  match u, v with
  | [], _ -> v
  | _, [] -> u
  | _ ->
      let len_u = List.length u in
      let delay = List.nth u (len_u - 1) in
      List.map (fun x -> x + delay) v

(* Fonction obs_concat_list : concatène une liste d'observables *)
let obs_concat_list (lists : 'a obs list) : 'a obs =
  List.fold_left obs_concat [] lists

(* Fonction obs_concat_map : applique une fonction à chaque élément d'un observable et concatène les résultats *)
let obs_concat_map (f : 'a -> 'b obs) (obs : 'a obs) : 'b obs =
  obs_concat_list (List.map f obs); 

  

  (* Type pour les observables *)
  type 'a obs = 'a list

  (* Fonction obs_merge : fusionne deux observables en respectant l'ordre chronologique *)
  let obs_merge (u : 'a obs) (v : 'a obs) : 'a obs =
    let rec merge_aux acc u v =
      match u, v with
      | [], [] -> List.rev acc
      | [], _ -> List.rev_append acc v
      | _, [] -> List.rev_append acc u
      | hd_u :: tl_u, hd_v :: tl_v ->
          if hd_u <= hd_v then merge_aux (hd_u :: acc) tl_u v
          else merge_aux (hd_v :: acc) u tl_v
    in
    merge_aux [] u v

  (* Fonction obs_merge_list : fusionne une liste d'observables *)
  let obs_merge_list (lists : 'a obs list) : 'a obs =
    List.fold_left obs_merge [] lists

  (* Fonction obs_merge_map : applique une fonction à chaque élément d'un observable et fusionne les résultats *)
  let obs_merge_map (f : 'a -> 'b obs) (obs : 'a obs) : 'b obs =
    obs_merge_list (List.map f obs)

