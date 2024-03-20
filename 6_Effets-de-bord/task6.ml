(* 1. Imprimer "Hello, world!" *)
let hello_world () : unit =
  print_endline "Hello, world!"

let print_int_option x = 
    match x with 
    |

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



















  (* Définition du type json *)
  type json =
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | Array of json list
    | Object of (string * json) list

  (* Définition de la fonction récursive print_json qui imprime une valeur de type json *)
  let rec print_json (j : json) : unit =
    match j with
    | Null -> print_endline "null"                   (* Si j est Null, imprime "null" *)
    | Bool b -> print_endline (if b then "true" else "false")   (* Si j est un Bool, imprime "true" si b est vrai, sinon "false" *)
    | Int i -> print_endline (string_of_int i)       (* Si j est un Int, imprime sa valeur entière *)
    | Float f -> print_endline (string_of_float f)   (* Si j est un Float, imprime sa valeur flottante *)
    | String s -> print_endline ("\"" ^ s ^ "\"")    (* Si j est une String, imprime la chaîne de caractères entre guillemets *)
    | Array lst ->                                   (* Si j est un Array, imprime ses éléments *)
        print_endline "[";                           (* Imprime un crochet ouvrant *)
        List.iter print_json lst;                    (* Itère sur chaque élément de la liste lst et imprime le JSON associé *)
        print_endline "]"                            (* Imprime un crochet fermant *)
    | Object lst ->                                  (* Si j est un Object, imprime ses paires clé-valeur *)
        print_endline "{";                           (* Imprime une accolade ouvrante *)
        List.iter (fun (k, v) -> print_endline (k ^ ": "); print_json v) lst; (* Pour chaque paire clé-valeur, imprime la clé suivie du JSON associé *)
        print_endline "}"                           (* Imprime une accolade fermante *)

  (* Exemple d'utilisation de la fonction print_json *)
  let example_json =
    Object [("name", String "John"); ("age", Int 30); ("verified", Bool true)] (* Crée un JSON représentant un objet *)





let rec fibonacci n =
if n <= 1 then n
else fibonacci (n - 1) + fibonacci (n - 2)


let fibonacci_memoized n =
let memo = Array.make (n + 1) None in
let rec fib_mem n =
  match memo.(n) with
  | Some x -> x
  | None ->
      let result =
        if n <= 1 then n
        else fib_mem (n - 1) + fib_mem (n - 2)
      in
      memo.(n) <- Some result;
      result
in
fib_mem n


let tableau = Array.init 100 (fun i -> Array.init 100 (fun j -> i + j))


