
(*Exercice II : Listes bien balisées*)

let rec taille (t:int) (n:int) : int =
if (n > 0) then (taille (2*t) (n-1))
else t
Q2
let rec duree (t:int) (m:int) : int =
if (t < m) then 1+(duree (2*t) m)
else 0
Version récursive terminale
let duree_term (t:int) (m:int) : int =
let rec loop (t:int) (res : int) : int =
if (t < m) then loop (2*t) (res+1) else res
in loop t 0
Q3
let rec taille2 (n:int) : int =
if (n=0) then 42
else let m = (taille2 (n-1)) in
if (m+m/2 > 128) then m/2
else m+m/2




let rec taille t n =
  if n = 0 then t
  else taille (2 * t) (n - 1)

(* Exemples *)
let exemple_q1_1 = taille 7 5 (* Résultat attendu : 224 *)
let exemple_q1_2 = taille 5 1 (* Résultat attendu : 10 *)
let exemple_q1_3 = taille 13 0 (* Résultat attendu : 13 *)



let rec duree t m =
  if t >= m then 0
  else 1 + duree (2 * t) m
  
(* Exemples *)
let exemple_q2_1 = duree 7 224 (* Résultat attendu : 5 *)
let exemple_q2_2 = duree 34 8 (* Résultat attendu : 0 *)


let rec taille2 n =
  let rec aux t =
    if t + (t / 2) > 128 then t / 2
    else t + (t / 2)
  in
  if n = 0 then 42
  else aux (taille2 (n - 1))

(* Exemples *)
let exemple_q3_1 = taille2 0 (* Résultat attendu : 42 *)
let exemple_q3_2 = taille2 1 (* Résultat attendu : 63 *)
let exemple_q3_3 = taille2 2 (* Résultat attendu : 94 *)
let exemple_q3_4 = taille2 3 (* Résultat attendu : 47 *)
let exemple_q3_5 = taille2 4 (* Résultat attendu : 70 *)



(*Exercice II : Listes bien balisées*)

Q1
let rec map_cons (e:’a) (l:’a list list) : (’a list list) =
match l with
[] -> []
| h::t -> (e::h) :: (map_cons e t);;
Q2
let rec prefixes (l:’a list) : ((’a list) list) =
match l with
[] -> [[]]
| h::t -> ([] :: (map_cons h (prefixes t)));
Q3
let add_fst (c:int*int) : (int * int) =
match c with
(a,b) -> (a+1,b)
let add_snd (c:int*int) : (int * int) = let (n1,n2)=c in (n1,n2+1)
Q4
let rec nb_of (l : char list) : int * int =
match l with
[] -> (0,0)
| x::xxs -> if (x = ’<’) then (add_fst (nb_of xxs)) else (add_snd (nb_of xxs))
Version récursive terminale
let nb_of_term (l : char list) : int * int =
let rec loop (l : char list) (res : int * int) : int * int =
match l with
[] -> res
| x::xxs -> if (x=’<’) then (loop xxs (add_fst res)) else (loop xxs (add_snd res))
in loop l (0,0)
Q5
let o_sup_f (l : char list) : bool =
let (bo,bf) = (nb_of l) in bo >= bf
5
Q6
let rec all_o_sup_f (l:(char list) list) : bool =
match l with
[] -> true
| h::t -> (o_sup_f h) && (all_o_sup_f t);;
Q7
let dyck (l:char list) : bool =
let (no,nf) = (nb_of l) in
(no=nf) && (all_o_sup_f (prefixes l));;




(*Exercice II : Listes bien balisées*)



(*Exercice II : Listes bien balisées*)



(*Exercice II : Listes bien balisées*)