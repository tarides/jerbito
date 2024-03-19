(* EXO 02  *)


let swap lst = match lst with
| [] | [_] -> lst 
| x :: y :: rest -> y :: x :: rest  



(* EXO 3  *)

let repeat element n =
if n < 0 then []  
else
  List.init n (fun _ -> element)


(* EXO 4 *)
let repeat_i_j i j =
if j < i then []  
else
  List.init (j-i +1)(fun n ->  i + n)

(* EXO 5 *)

let decr_list lst =
List.map (fun x -> x - 1) lst

(* EXO 6  *)
let rec rev lst =
let rec rev_acc acc = function
  | [] -> acc
  | hd :: tl -> rev_acc (hd :: acc) tl
in
rev_acc [] lst


let rev l = 
  let rec rev_aux acc rest  = 
    match rest with 
    | [] -> acc
    | t :: q -> rev_aux ( t :: acc) 
  in
rev_aux [] l
  
L'accumulateur, C'est un notion de PF, 
Elle va avoir besoin de se rappeler de l'état intermediare 
(* EXO 7 *)
let rec flat lst =
match lst with
| [] -> []  
| hd :: tl -> hd @ flat tl 

