let char_succ =
if c < '\255' then 
 c |> int_of_char |> succ |> char_of_int
  else 
    '\000';;


let cr = "scream" in
let i_u_we = "I scream, you scream, we all " ^ cr in
let result = i_u_we ^ " for ice " in

result;;


let rec char_range lo hi =
  if lo > hi then
    ""
  else
    String.make 1 lo ^ char_range (char_succ lo) hi
;;



1. Écrire une fonction qui prend en paramètre un entier n et qui
(* Exemple d'utilisation *)
char_range 'a' 'd';;
(* Résultat: "abcd" *)


let rec char_range_rev_cat s lo hi =
  if lo > hi then
    s
  else
    let prev_char = Char.chr (Char.code hi - 1) in
    char_range_rev_cat (String.make 1 hi ^ s) lo prev_char
;;

Devoir à faire
(* Exemple d'utilisation *)
char_range_rev_cat "123" 'a' 'd';;
(* Résultat: "dcba123" *)


let char_range_rev lo hi =
  let rec aux acc lo hi =
    if lo > hi then
      acc
    else
      let prev_char = Char.chr (Char.code hi - 1) in
      aux (acc ^ String.make 1 hi) lo prev_char
  in
  aux "" lo hi
;;

char_range_rev 'a' 'd';;
(* Résultat: "dcba" *)


let rec is_palindrome s =
  let len = String.length s in
  if len <= 1 then
    true  (* Une chaîne vide ou une chaîne d'un seul caractère est toujours un palindrome *)
  else if s.[0] = s.[len - 1] then
    is_palindrome (String.sub s 1 (len - 2))  (* Comparaison réussie, récursion à l'intérieur de la sous-chaîne *)
  else
    false  (* Comparaison échouée, la chaîne n'est pas un palindrome *)
;;

is_palindrome "tenet";;
(* Résultat: true *)

is_palindrome "opera";;
(* Résultat: false *)








