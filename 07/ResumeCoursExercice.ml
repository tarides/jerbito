 module IntSet = Set.Make(struct type t = int let compare a b = a -b end);;

let has_dupes lst =
  let rec aux set = function
    | [] -> false
    | x :: xs -> if IntSet.mem x set then true else aux (IntSet.add x set) xs
  in
  aux IntSet.empty lst


  (* Vérifie si une liste contient des doublons *)
  let has_dupes lst =
    (* Définition d'une fonction récursive auxiliaire prenant un ensemble et une liste en entrée *)
    let rec aux set = function
      (* Cas de base : la liste est vide, il n'y a donc pas de doublons *)
      | [] -> false
      (* Cas récursif : la liste n'est pas vide, on vérifie si la tête de la liste est déjà présente dans l'ensemble *)
      | x :: xs -> if IntSet.mem x set then true else aux (IntSet.add x set) xs
    (* Appel de la fonction auxiliaire avec un ensemble vide et la liste en entrée *)
    in
    aux IntSet.empty lst

















let uniq lst =
  let rec aux set acc = function
    | [] -> List.rev acc
    | x :: xs -> if IntSet.mem x set then aux set acc xs else aux (IntSet.add x set) (x :: acc) xs
  in
  aux IntSet.empty [] lst























module StringSet = Set.Make(String)

let has_dupes lst =
  let rec aux set = function
    | [] -> false
    | x :: xs -> if StringSet.mem x set then true else aux (StringSet.add x set) xs
  in
  aux StringSet.empty lst

let uniq lst =
  let rec aux set acc = function
    | [] -> List.rev acc
    | x :: xs -> if StringSet.mem x set then aux set acc xs else aux (StringSet.add x set) (x :: acc) xs
  in
  aux StringSet.empty [] lst
















module IntPairSet = Set.Make(struct type t = int * int let compare = compare end)

module Int2Set = struct
  let split set =
    IntPairSet.fold (fun (x, y) (set_x, set_y) -> IntSet.add x set_x, IntSet.add y set_y) set (IntSet.empty, IntSet.empty)

  let combine set1 set2 =
    IntSet.fold (fun x acc -> IntSet.fold (fun y acc -> IntPairSet.add (x, y) acc) set2 acc) set1 IntPairSet.empty
end



















module IntSet = Set.Make(struct type t = int let compare = compare end)

(* Vérifie si une liste contient des doublons *)
let has_dupes lst =
  let rec aux set = function
    | [] -> false
    | x :: xs -> if IntSet.mem x set then true else aux (IntSet.add x set) xs
  in
  aux IntSet.empty lst

(* Supprime les doublons d'une liste *)
let uniq lst =
  let rec aux set acc = function
    | [] -> List.rev acc
    | x :: xs -> if IntSet.mem x set then aux set acc xs else aux (IntSet.add x set) (x :: acc) xs
  in
  aux IntSet.empty [] lst






module StringSet = Set.Make(String)

(* Vérifie si une liste contient des doublons *)
let has_dupes lst =
  let rec aux set = function
    | [] -> false
    | x :: xs -> if StringSet.mem x set then true else aux (StringSet.add x set) xs
  in
  aux StringSet.empty lst

(* Supprime les doublons d'une liste *)
let uniq lst =
  let rec aux set acc = function
    | [] -> List.rev acc
    | x :: xs -> if StringSet.mem x set then aux set acc xs else aux (StringSet.add x set) (x :: acc) xs
  in
  aux StringSet.empty [] lst






module IntPairSet = Set.Make(struct type t = int * int let compare = compare end)

module Int2Set = struct
  (* Sépare un ensemble de paires d'entiers en deux ensembles distincts *)
  let split set =
    IntPairSet.fold (fun (x, y) (set_x, set_y) -> IntSet.add x set_x, IntSet.add y set_y) set (IntSet.empty, IntSet.empty)

  (* Combine deux ensembles d'entiers en un ensemble de paires d'entiers *)
  let combine set1 set2 =
    IntSet.fold (fun x acc -> IntSet.fold (fun y acc -> IntPairSet.add (x, y) acc) set2 acc) set1 IntPairSet.empty
end

















module CharMap = Map.Make(Char)

let char_digit n =
  if n >= 0 && n <= 9 then
    let char_code_0 = Char.code '0' in
    let digit_char = Char.chr (char_code_0 + n) in
    
    digit_char
  else
    failwith "Invalid digit"

(* Teston la fonction char_digit *)
let () =
  for i = 0 to 9 do
    Printf.printf "char_digit %d = %c\n" i (char_digit i)
  done






















let init =
  let digits = List.init 10 char_digit in
  let pairs = List.mapi (fun i c -> (c, 0)) digits in
  CharMap.of_list pairs

let lead_digit n =
  let n_str = string_of_int n in
  if String.length n_str > 0 then
    n_str.[0]
  else
    failwith "Empty string"

let record_lead_digit dico n =
  let leading_digit = lead_digit n in
  let count =
    match CharMap.find_opt leading_digit dico with
    | Some c -> c + 1
    | None -> 1
  in
  CharMap.add leading_digit count dico

(* Test *)
let updated_dico = record_lead_digit init 713
