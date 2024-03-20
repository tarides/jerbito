(* 1. Imprimer "Hello, world!" *)
let hello_world () : unit =
  print_endline "Hello, world!"

(* 3. Imprimer un entier optionnel *)
let print_int_option (opt : int option) : unit =
  match opt with
  | None -> print_endline "None"
  | Some i -> print_endline (string_of_int i)

(* 4. Imprimer une valeur de type json *)
type json =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let rec print_json (j : json) : unit =
  match j with
  | Null -> print_endline "null"
  | Bool b -> print_endline (if b then "true" else "false")
  | Int i -> print_endline (string_of_int i)
  | Float f -> print_endline (string_of_float f)
  | String s -> print_endline ("\"" ^ s ^ "\"")
  | Array lst ->
      print_endline "[";
      List.iter print_json lst;
      print_endline "]"
  | Object lst ->
      print_endline "{";
      List.iter (fun (k, v) -> print_endline (k ^ ": "); print_json v) lst;
      print_endline "}"

(* Exemple d'utilisation *)
let example_json =
  Object [("name", String "John"); ("age", Int 30); ("verified", Bool true)]

(* Appel de la fonction pour imprimer le JSON *)
let () = print_json example_json


(* 1. Fonction récursive pour calculer la suite de Fibonacci *)
let rec fibonacci (n : int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (n - 1) + fibonacci (n - 2)

(* 2. Fonction pour calculer la suite de Fibonacci en utilisant la mémoïsation *)
let fibonacci_memoized (n : int) : int =
  let memo = Array.make (n + 1) None in
  let rec fib (m : int) : int =
    match memo.(m) with
    | Some x -> x
    | None ->
        let result =
          match m with
          | 0 -> 0
          | 1 -> 1
          | _ -> fib (m - 1) + fib (m - 2)
        in
        memo.(m) <- Some result;
        result
  in
  fib n

(* 3. Création d'un tableau à double entrée de taille 100x100 *)
let tableau_2d : int array array =
  let n = 100 in
  Array.init n (fun i -> Array.init n (fun j -> i + j))
