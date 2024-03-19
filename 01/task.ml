(* Ex O1 *)
let char_succ c = 
  if c < '\255' then c |> int_of_char |> succ |> char_of_int
  else 
  '\000'


(* Ex O2 *)
let string_cons c s = String.make 1 c ^ s


(* EX 03 *)
let _ = int_of_float (sqrt (float_of_int (int_of_string "81")))

let _ =
  let string_to_int = int_of_string "81" in
  let int_to_float = float_of_int string_to_int in
  let square_root = sqrt int_to_float in
  let d = int_of_float square_root in 


int_of_float (sqrt (float_of_int (int_of_string "81")))


(* EX 04 *)
let _ = 
  let cr = "cream" in
  let i_u_we = 
    let sr = string_cons 's' cr in
    "I " ^ sr ^ ", you " ^ sr ^ ", we all " ^ sr in
  i_u_we ^ " for ice " ^ cr


(* Pour info  *)
let f = let c = ref 0 in fun x -> c := !c +  x; !c

(* Fonctions Récursives *)
(* EX1 *)

let rec char_range lo hi =
  if lo > hi then
    ""
  else
    let next_char = char_succ lo in
    string_cons lo (char_range next_char hi)



(* Lists *)
let swap lst = match lst with
| [] | [_] -> lst 
| x :: y :: rest -> y :: x :: rest  

let repeat s n = 
if n < 0 then []
else List.init s(fun _ -> s)
