(* -- Q1 *)
(* La fonction access1 prend un élément s et une liste de paires (x,y) et retourne une liste des éléments y associés à s. *)
let rec access1 (s:’a) (l:(’a*’a) list) : ’a list =
  match l with
  | [] -> []
  | (x,y)::t -> if s=x then y::(access1 s t) (* Si l'élément s correspond à x, ajoute y à la liste et continue la recherche *)
                else (access1 s t);; (* Sinon, continue la recherche *)

(* -- Q2 *)
(* La fonction list_access1 prend une liste ls et une liste de paires (x,y) et retourne une liste des éléments y associés à chaque élément de ls. *)
let rec list_access1 (ls:’a list) (l:(’a*’a) list) : ’a list =
  match ls with
  | [] -> []
  | h::t -> (access1 h l) @ (list_access1 t l);; (* Applique access1 à chaque élément de ls et concatène les résultats *)

(* -- Q3 *)
(* La fonction access2 prend un élément s et une liste de paires (x,y) et retourne une liste des éléments y associés à l'élément s. *)
let access2 (s:’a) (l:(’a*’a) list) : ’a list =
  (list_access1 (access1 s l) l);; (* Applique access1 pour obtenir les éléments associés à s, puis applique list_access1 *)

(* -- Q4 *)
(* La fonction accessn prend un élément s, un entier n et une liste de paires (x,y) et retourne une liste des éléments y associés à s répétés n fois. *)
let rec accessn (s:’a) (n:int) (l:(’a*’a) list) : ’a list =
  if (n<0) then []
  else if (n=0) then [s] (* Cas de base, n = 0 *)
  else if (n=1) then (access1 s l) (* Cas de base, n = 1 *)
  else (list_access1 (accessn s (n-1) l) l);; (* Appel récursif pour n > 1 *)

(* Version récursive terminale *)
let accessn (s:’a) (n:int) (l:(’a*’a) list) : ’a list =
  let rec loop n r =
    if (n=0) then r
    else loop (n-1) (list_access1 r l) (* Utilisation d'une fonction auxiliaire récursive terminale *)
  in
  if (n < 0) then []
  else (loop n [s]);;

(* -- Q5 *)
(* La fonction access_infn prend un élément s, un entier n et une liste de paires (x,y) et retourne une liste des éléments y associés à s répétés n fois, en incluant toutes les étapes intermédiaires. *)
let rec access_infn (s:’a) (n:int) (l:(’a*’a) list) : ’a list =
  if n=0 then raise (Invalid_argument "bad access_infn")
  else if n=1
  then (access1 s l) (* Cas de base, n = 1 *)
  else (accessn s n l) @ (access_infn s (n-1) l);; (* Appel de accessn et récursivité pour n > 1 *)

(* -- Q6 *)
(* La fonction cycle_from prend un élément s et une liste de paires (x,y) et retourne true si s est présent dans un cycle infini, sinon false. *)
let cycle_from (s:’a) ((l:(’a*’a) list)) : bool =
  (List.mem s (access_infn s (List.length l) l));; (* Utilise access_infn pour vérifier si s est dans un cycle infini *)



avant 




type bin = O | I of bin;;

let rec int_to_bin (i : int) : bin =
  if i = 0 then O
  else if i mod 2 = 0 then I (int_to_bin (i / 2))
  else I (int_to_bin (i / 2))


  (* -- Q1 *)
  let rec btree_of_bin (m:bin list) : bool btree =
  match m with
  [] -> Node(true, Empty, Empty)
  | I::m -> Node(false, btree_of_bin m, Empty)
  | O::m -> Node(false, Empty, btree_of_bin m)
  (* -- Q2 *)
  let rec appartient (m:bin list) (bt:bool btree) : bool =
  match m, bt with
  _, Empty -> false
  | [], Node(x,_,_) -> x
  | I::m, Node(_,bt1,_) -> appartient m bt1
  | O::m, Node(_,_,bt2) -> appartient m bt2
  (* -- Q3 *)
  let rec ajout (m:bin list) (bt:bool btree) : bool btree =
  match bt with
  Empty -> btree_of_bin m
  | Node(x,bt1,bt2) -> (
  match m with
  [] -> Node(true, bt1, bt2)
  | I::m -> Node(x, ajout m bt1, bt2)
  | O::m -> Node(x, bt1, ajout m bt2)
  )
  (* -- Q4 *)
  let rec retrait (m:bin list) (bt:bool btree) : (bool btree) =
  match bt with
  Empty -> Empty
  | Node(x,bt1,bt2) -> (
  match m with
  [] -> Node(false,bt1,bt2)
  | I::m -> Node(x, retrait m bt1, bt2)
  | O::m -> Node(x, bt1, retrait m bt2)
  )
