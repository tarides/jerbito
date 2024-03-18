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

Devoir à faire : 

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


let rec char_range lo hi =
  if lo > hi then
    ""
  else
    String.make 1 lo ^ char_range (char_succ lo) hi
;;
Devoir à faire


let rec char_range_rev_cat s lo hi =
  if lo > hi then
    s
  else
    char_range_rev_cat (String.make 1 lo ^ s) (Char.chr (Char.code lo + 1)) hi



let char_range_rev lo hi =
  let rec aux acc lo hi =
    if lo > hi then
      acc
    else
      aux (string_cons hi acc) lo (char_pred hi)
  in
  aux "" lo hi
;;

(* Exemple d'utilisation *)
char_range_rev 'a' 'd';;
(* Résultat: "dcba" *)
;;


