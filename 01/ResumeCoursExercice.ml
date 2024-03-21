
              (* RESUME COURS 1 *)................

let my_list = [1; 2; 3; 4; 5]
let length = List.length my_list (* Obtient la longueur de la liste *)
let reversed = List.rev my_list (* Inverse la liste *)


let my_string = "Hello, World!"
let length = String.length my_string (* Obtient la longueur de la chaîne *)
let uppercase = String.uppercase_ascii my_string (* Convertit la chaîne en majuscules *)

let my_array = [|1; 2; 3; 4; 5|]
let nth_element = Array.get my_array 2 (* Obtient le troisième élément du tableau *)
let sorted_array = Array.sort compare my_array (* Trie le tableau *)

let sum = Int.add 5 3 (* Addition *)
let difference = Int.sub 8 4 (* Soustraction *)
let product = Int.mul 2 6 (* Multiplication *)
let quotient = Int.div 10 2 (* Division *)
let remainder = Int.rem 7 3 (* Modulo *)

let shifted = Int.shift_left 3 2 (* Décalage à gauche de 2 bits *)
let masked = Int.logand 5 3 (* Opération de masquage *)
let toggled = Int.logxor 7 3 (* Opération de basculement de bits *)


let compare_result = Int.compare 5 8 (* Compare deux entiers *)
let is_positive = Int.gt 5 0 (* Vérifie si le premier entier est strictement supérieur au deuxième *)
let is_even = Int.even 6 (* Vérifie si un entier est pair *)


let compare_result = Int.compare 5 8 (* Compare deux entiers *)
let is_positive = 5 > 0 (* Vérifie si le premier entier est strictement supérieur au deuxième *)
let is_even = 6 mod 2 = 0 (* Vérifie si un entier est pair *)

let b1 = true in
let b2 = false in
let result = b1 && b2 in
result;; (* Affiche le résultat *)

let result =
  let b1 = true in
  let b2 = false in
  b1 && b2 in
result;; (* Affiche le résultat *)


let s = "Hello, World!" in
let len = String.length s in
print_endline (string_of_int len) (* Affiche la longueur de la chaîne *)

let s1 = "Hello, " in
let s2 = "World!" in
let combined = s1 ^ s2 in
print_endline combined (* Affiche "Hello, World!" *)

let n = 42 in
let s = string_of_int n in
print_endline s (* Affiche "42" *)

let s = "Hello" in
let first_char = s.[0] in
print_char first_char (* Affiche 'H' *)

let c = 'A' in
let code = int_of_char c in
print_int code (* Affiche 65 *)


(* Etudes sur les fonctions : *)

let add x y = x + y;;

let rec factorial n =
if n <= 1 then 1
else n * factorial (n - 1);;

let square = fun x -> x * x;;

let add_one x = x + 1;;
let apply_function f x = f x;;

let result = apply_function add_one 5;;



let add = fun x -> (fun y -> x + y);;

let add_five = add 5;;
let result = add_five 3;; (* result vaut 8 *)


(*Les fonctions recursives :*)
let rec char_range lo hi =
if lo > hi then ""
else string_cons lo (char_range (char_succ lo) hi)


(*EXERCICE*)

### Fonctions récursives

1. En utilisant les fonction `char_succ` et `string_cons` définies précédemment,
   écrire une fonction `char_range` de type `char -> char -> string` telle que
   `char_range lo hi` renvoie une chaîne de caractères formé de tous les
   caractères de `lo` à `hi` (en ordre croissant). Par exemple, `char_range 'a'
   'd'` renvoie `"abcd"`.

   (* Définition de la fonction char_succ *)
   let char_succ c = Char.chr (Char.code c + 1)

   (* Définition de la fonction string_cons *)
   let string_cons c s = String.make 1 c ^ s

   (* Définition de la fonction char_range *)
   let rec char_range lo hi =
     if lo > hi then ""
     else string_cons lo (char_range (char_succ lo) hi)

   (* Utilisation de la fonction char_range *)
   let result = char_range 'a' 'd';;


1. En utilisant les fonction `char_succ` et `string_cons` définies précédemment,
   écrire une fonction `char_range_rev_cat` de type `string -> char -> char ->
   string` telle que `char_range s lo hi` renvoie une chaîne de caractères formé
   de tous les caractères de `hi` à `lo` (en ordre décroissant) puis ceux de
   `s`. Par exemple, `char_range_rev_cat "123" 'a' 'd'` renvoie `"dcba123"`.


(* Définition de la fonction char_pred *)
let char_pred c = Char.chr (Char.code c - 1)

(* Définition de la fonction char_succ *)
let char_succ c = Char.chr (Char.code c + 1)

(* Définition de la fonction string_cons *)
let string_cons c s = String.make 1 c ^ s

(* Définition de la fonction char_range_rev_cat *)
let rec char_range_rev_cat s lo hi =
  if lo > hi then s
  else char_range_rev_cat (string_cons hi s) lo (char_pred hi)

(* Utilisation de la fonction char_range_rev_cat *)
let result = char_range_rev_cat "123" 'a' 'd';;


1. En réutilisant le corps de la fonction précédente et une définition locale,
   écrire une fonction `char_range_rev` de type `char -> char -> string` telle
   que `char_range lo hi` renvoie une chaîne de caractères formé de tous les
   caractères de à `hi` à `lo` (en ordre décroissant). Par exemple,
   `char_range_rev 'a' 'd'` renvoie `"dcba"`.

(* Définition de la fonction char_pred *)
let char_pred c = Char.chr (Char.code c - 1)

(* Définition de la fonction char_succ *)
let char_succ c = Char.chr (Char.code c + 1)

(* Définition de la fonction string_cons *)
let string_cons c s = String.make 1 c ^ s

(* Définition de la fonction char_range_rev *)
let char_range_rev lo hi =
  let rec char_range_rev_helper lo hi acc =
    if lo > hi then acc
    else char_range_rev_helper (char_succ lo) hi (string_cons lo acc)
  in
  char_range_rev_helper lo hi ""

(* Utilisation de la fonction char_range_rev *)
let result = char_range_rev 'a' 'd';;


1. Écrire une fonction recursive `is_palindrome` de type `string -> bool` telle
   que `is_palindrome s` ne renvoie `true` que si dans `s` : le premier
   caractère de est égal au dernier, le deuxième à l'avant dernier, _etc._ Par
   exemple, `is_palindrome "tenet"` vaut `true` alors que `is_palindrome
   "opera"` vaut `false`.

let rec is_palindrome s =
  let len = String.length s in
  if len <= 1 then true
  else
    let first_char = s.[0] in
    let last_char = s.[len - 1] in
    if first_char = last_char then
      is_palindrome (String.sub s 1 (len - 2))
    else
      false

(* Tests *)
let () =
  assert (is_palindrome "tenet" = true);
  assert (is_palindrome "opera" = false)



1. Écrire une fonction récursive `pow` de type `int -> int -> int` telle que
   `pow x n` calcule _x<sup>n</sup>_. Cette fonction doit utiliser l'algorithme
   de l'exponentiation rapide:
     - _x<sup>0</sup> = 1_
     - _x<sup>n</sup> = (x<sup>n/2</sup>)<sup>2</sup>_ si _n_ est pair
     - _x<sup>n</sup> = x &times; (x<sup>n/2</sup>)<sup>2</sup>_ si _n_ est impair


(* Définition de la fonction is_palindrome *)
let rec is_palindrome s =
  (* Calcul de la longueur de la chaîne *)
  let len = String.length s in
  (* Si la longueur est inférieure ou égale à 1, la chaîne est un palindrome *)
  if len <= 1 then true
  else
    (* Extraction du premier et du dernier caractère de la chaîne *)
    let first_char = s.[0] in
    let last_char = s.[len - 1] in
    (* Vérification si les premiers et derniers caractères sont égaux *)
    if first_char = last_char then
      (* Récursion sur le reste de la chaîne, en retirant le premier et le dernier caractère *)
      is_palindrome (String.sub s 1 (len - 2))
    else
      (* Si les premiers et derniers caractères ne correspondent pas, la chaîne n'est pas un palindrome *)
      false

(* Tests *)
let () =
  (* Vérification du palindrome "tenet" *)
  assert (is_palindrome "tenet" = true);
  (* Vérification du non-palindrome "opera" *)
  assert (is_palindrome "opera" = false)


1. Écrire une fonction récursive `div` de type `int -> int -> int` telle que
   `div n d` calcule _n/d_ en utilisant l'algorithme d'Euclide:
     - _n/d = 0_ si _n < d_
     - _n/d = 1 + (n - d) / d_ si _n >= d_


(* Définition de la fonction div *)
let rec div n d =
  (* Vérification si n est inférieur à d *)
  if n < d then
    (* Si n est inférieur à d, le quotient est 0 *)
    0
  else
    (* Calcul du quotient en ajoutant 1 au quotient précédent *)
    1 + div (n - d) d

(* Tests *)
let () =
  (* Vérification de div(10, 2) *)
  assert (div 10 2 = 5);
  (* Vérification de div(7, 3) *)
  assert (div 7 3 = 2);
  (* Vérification de div(15, 4) *)
  assert (div 15 4 = 3)
