(* 1. Imprimer "Hello, world!" *)

let hello_world () : unit =
  print_endline "Hello, world!";;

  (* 3. Imprimer un entier optionnel *)
  let print_int_option (opt : int option) : unit =
    match opt with
    | None -> print_endline " foncton None  "
    | Some i -> print_endline (string_of_int i);; 

   (* 4. Imprimer une valeur de type json *)
type json =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list;;

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
      print_endline "}";;



(* Tableau et reference *)

let rec fibonacci n =
if n <= 1 then n
else fibonacci (n - 1) + fibonacci (n - 2);;

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
fib_mem n;;




































      

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