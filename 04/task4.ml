(* EXERCICE 1:  *)

let first (a, _) = a;;
let second (_, b) = b;;

(* EXERCICE 2:  *)
let sum (a, x) (b, y) = (a + b, x ^ y);;

(* EXERCICE 3:  *)

let suffix_prefix str c =
  let rec find_suffix_prefix str c prev_char =
    match str with
    | "" -> (None, None)  (* Aucune occurrence de c dans la chaîne *)
    | hd :: tl ->
      if hd = c then
        (prev_char, match tl with (* Retourne le caractère suivant s'il existe *)
                    | [] -> None
                    | next_char :: _ -> Some next_char)
      else
        find_suffix_prefix tl c (Some hd) (* Recherche récursive *)
  in
  find_suffix_prefix (String.to_list str) c None
;;

(* Exemples *)
suffix_prefix "Bonjour" 'o';;
(* Résultat: (Some 'B', Some 'n') *)

suffix_prefix "Bonjour" 'B';;
(* Résultat: (None, Some 'o') *)

suffix_prefix "Bonjour" 'r';;
(* Résultat: (Some 'u', None) *)

suffix_prefix "Bonjour" 'a';;
(* Résultat: (None, None) *)




(* EXERCICE 4:  *)


exception Listes_de_taille_different

let zip lst1 lst2 =
  let rec zip_aux acc lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> List.rev acc
    | [], _ | _, [] -> raise Listes_de_taille_different
    | hd1 :: tl1, hd2 :: tl2 -> zip_aux ((hd1, hd2) :: acc) tl1 tl2
  in
  zip_aux [] lst1 lst2;;

let rec unzip lst =
  match lst with
  | [] -> ([], [])
  | (x, y) :: tl ->
    let xs, ys = unzip tl in
    (x :: xs, y :: ys);;

zip [1;2;3] [4;5;6];;

unzip [(1,1); (2, 3); (4, 3)];;




(* EXERCICE 5:  *)

exception Empty
let rec min_max_recursive lst =
  match lst with
  | [] -> raise Empty
  | [x] -> (x, x)
  | hd :: tl ->
      let (min_tail, max_tail) = min_max_recursive tl in
      (min hd min_tail, max hd max_tail);;

(* Implémentation avec List.fold_left *)

let min_max_fold lst =
  match lst with
  | [] -> raise Empty
  | hd :: tl ->
      let min_val, max_val =
        List.fold_left
          (fun (min_acc, max_acc) x -> (min min_acc x, max max_acc x))
          (hd, hd) tl
      in
      (min_val, max_val);;

let example_list = [5; 3; 9; 1; 7]
let result_recursive = min_max_recursive example_list
let result_fold = min_max_fold example_list 
