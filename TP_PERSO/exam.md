

Exercice I : Suites numériques

Q1 – (2pts) On définit la suite u de paramètres c, t, m de la manière suivante:

Définir la fonction

u (n:int) (c:float) (t:float) (m:float) : float
qui donne le n-ième terme de la suite un. Solution Exercice I
let rec u (n:int) (c:float) (t:float) (m:float) : float =
if (n=0) then c
else ((1.+.t)*.(u (n-1) c t m)) -. m
Fin Solution

Exercice II : Distance de Hamming

La distance de Hamming donne une mesure de la différence entre deux séquences de bits. C’est le nombre de positions où les deux séquences diffèrent. On l’utilise en transmission réseau pour des codes correcteurs.
Q1 – (2pts) Définir la fonction
nbtrue (bs:bool list) : int
qui donne le nombre de fois où bs contient la valeur true.
Bonus: +1pt pour une définition récursive terminale ou l’utilisation de List.fold_left.
Q2 – (2pts) Définir la fonction
sumb (xs:’a list) (ys:’a list) : bool list
qui donne la liste des booléens qui contient true à chaque position où xs et ys diffèrent et false à toutes les autres positions. De plus, la fonction déclenche l’exception Invalid_argument "sumb" si les deux listes n’ont pas la même longueur.
Par exemple :
(sumb [0;0;1;0;1;0] [0;1;1;0;0;1]) vaut [false; true; false; false; true; true]
(sumb [] [0;0]) donne Invalid_argument "sumb" Malus: -1pt si vous utilisez la fonction List.length.
Q3 – (1pts) Déduire des deux questions précédentes la définition de la fonction
disth (xs:’a list) (ys:’a list) : int
qui donne la distance de Hamming entre xs et ys.
Solution Exercice II
(* Q1 *)
let rec nbtrue (bs:bool list) : int =
match bs with
[] -> 0
| true::bs -> 1+(nbtrue bs)
| _::bs -> (nbtrue bs)
(* recursif terminal *) let nbtrue_rt (bs:bool list) : int =
let rec loop bs r =
match bs with
[] -> r
| true::bs -> 1+(nbtrue bs)
| _::bs -> (nbtrue bs) in
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
let disth (xs:’a list) (ys:’a list) : int = (nbtrue (sumb xs ys))
Fin Solution
Exercice III : Tri rapide

Le principe de l’algorithme de tri dit rapide appliqué à une liste xs est le suivant:
•	si xs est vide, le résultat est []
•	si xs = x::xs’ alors le résultat est la liste (xs1 @ [x] @ xs2) où xs1 est le résultat du tri des éléments de xs’ inférieurs ou égaux à x et xs2 est le résultat du tri des éléments de xs’ strictement supérieurs à x
On va utiliser cette méthode pour trier des listes.
Q1 – (2pts) Définir les fonctions
list_le (x:’a) (xs:’a list) : ’a list list_gt (x:’a) (xs:’a list) : ’a list
telles que (list_le x xs) donne la liste des éléments de xs inférieurs ou égaux à x et (list_gt x xs) donne la liste des éléments de xs strictement supérieurs à x
Bonus: +1pt si vous utilisez l’itérateur List.filter
Q2 – (3pts) Définir la fonction
qsort (xs:’a list) : ’a list
qui donne la liste triée des éléments de xs en utilisant la méthode du tri rapide.
Solution Exercice III
(* Q1 *) let rec list_le (x:int) (xs:int list) : int list =
match xs with
[] -> []
| y::xs -> if (y <= x) then y::(list_le x xs) else (list_le x xs)
let rec list_gt (x:int) (xs:int list) : int list =
match xs with
[] -> []
| y::xs -> if (y > x)then y::(list_gt x xs) else (list_gt x xs)
(* filter *)
let list_le (x:int) (xs:int list) : int list =
let le_x y = y <= x in
List.filter le_x xs
let list_gt (x:int) (xs:int list) : int list =
let gt_x y = y > x in
List.filter gt_x xs (* Q2 *)
let rec qsort (xs:int list) : int list =
match xs with
[] -> []
| x::[] -> xs
| x::xs -> (qsort (list_le x xs)) @ [x] @ (qsort (list_gt x xs))
Fin Solution
Exercice IV : Différences dans une liste
Dans toutes les questions de cet exercice, utilisez la fonction List.mem.
Q1 – (2pts) Définir la fonction
all_diff (xs:’a list) : bool
qui donne true si tous les éléments de xs sont différents entre eux.
Par convention, (all_diff [])=true.
Q2 – (2pts) Définir la fonction
nb_diff (xs:’a list) : int
qui donne le nombre d’éléments différents entre eux de xs.
Par exemple :
(nb_diff []) vaut 0
(nb_diff [42; 42; 42]) vaut 1
(nb_diff [42; 54; 42; 42; 54]) vaut 2
Q3 – (3pts) Pour savoir qu’une liste contient au moins n valeurs différentes entre elles il n’est pas efficace d’utiliser la fonction nb_diff. En effet, on peut définir directement la fonction at_least_n_diff telle que :
(at_least_n_diff 0 xs)	=	true	
(at_least_n_diff n [])	=	false	si n6= 0
(at_least_n_diff n (x::xs))	=	(at_least_n_diff n xs)	si n6= 0 et x est dans xs
(at_least_n_diff n (x::xs))	=	(at_least_n_diff (n-1) xs)	si n6= 0 et x n’est pas dans xs
Définir la fonction
at_least_n_diff (n:int) (xs:’a list) : bool
en OCaml.
Q4 – (1pts) Si n < 0, l’application (at_least_n_diff n xs) a-t-elle une valeur ? Si oui, laquelle; sinon expliquez pourquoi.
Solution Exercice IV
let rec all_diff (xs:’a list) : bool =
match xs with
[] -> true
| x::xs -> (not (List.mem x xs)) && (all_diff xs)
let rec nb_diff (xs:’a list) : int =
match xs with
[] -> 0
| x::xs -> if (List.mem x xs) then (nb_diff xs) else 1+(nb_diff xs)
let rec at_least_n_diff (n:int) (xs:’a list) : bool =
if (n=0) then true else match xs with
[] -> false
| x::xs -> if (List.mem x xs) then (at_least_n_diff n xs) else (at_least_n_diff (n-1) xs)
let _ = assert ((at_least_n_diff (-1) [7;8;7;0;5;4;5])==false)
Fin Solution
