(* Fonction de permutation de deux éléments dans une liste *)
let swap_elements lst i j =
  let rec swap_aux lst i j =
    match lst with
    | [] -> []
    | x :: xs when i = 0 -> (
        match xs with
        | [] -> [List.nth lst j]
        | y :: ys -> (List.nth lst j) :: ys @ [x]
      )
    | x :: xs when j = 0 -> (
        match xs with
        | [] -> [List.nth lst i]
        | y :: ys -> (List.nth lst i) :: ys @ [x]
      )
    | x :: xs -> x :: swap_aux xs (i - 1) (j - 1)
  in
  swap_aux lst (min i j) (max i j)

(* Exemple d'utilisation *)
let int_list = [1; 2; 3; 4; 5]
let string_list = ["apple"; "banana"; "orange"]

let swapped_int_list = swap_elements int_list 0 4
let swapped_string_list = swap_elements string_list 1 2

(* Affichage des résultats *)
let () =
  Printf.printf "Liste d'entiers après permutation : [%s]\n" (String.concat "; " (List.map string_of_int swapped_int_list));
  Printf.printf "Liste de chaînes après permutation : [%s]\n" (String.concat "; " swapped_string_list);;









  (* Définition d'une fonction générique pour retourner le premier élément d'une paire *)
  let premier_element (x, _) = x

  (* Définition de différentes paires *)
  let paire_int = (10, 20)
  let paire_float = (3.14, 2.71)
  let paire_string = ("Bonjour", "Monde")

  (* Utilisation de la fonction avec différentes paires *)
  let premier_int = premier_element paire_int (* Retourne 10 *)
  let premier_float = premier_element paire_float (* Retourne 3.14 *)
  let premier_string = premier_element paire_string (* Retourne "Bonjour" *)

  (* Affichage des résultats *)
  let () =
    Printf.printf "Premier élément de la paire d'entiers : %d\n" premier_int;
    Printf.printf "Premier élément de la paire de flottants : %f\n" premier_float;
    Printf.printf "Premier élément de la paire de chaînes : %s\n" premier_string


(* Définition d'un type pour les timbres temporels *)
type 'a tstamp = int * 'a

(* Fonction tstamp_join : fusionne deux timbres temporels en prenant la date la plus récente *)
let tstamp_join (ts1 : 'a tstamp) (ts2 : 'a tstamp) : 'a tstamp =
  if fst ts1 > fst ts2 then ts1 else ts2

(* Fonction tstamp_dupe : duplique un timbre temporel *)
let tstamp_dupe (ts : 'a tstamp) : 'a tstamp tstamp =
  (ts, ts)

(* Fonction tstamp_map : applique une fonction à la valeur du timbre temporel *)
let tstamp_map (f : 'a -> 'b) (ts : 'a tstamp) : 'b tstamp =
  (fst ts, f (snd ts))
