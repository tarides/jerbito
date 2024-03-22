type ’a btree =
Empty
| Node of ’a * ’a btree * ’a btree
(* Q1 *)
let rec cons_all (x:’a) (xss:(’a list) list) : (’a list) list =
match xss with
[] -> []
| xs::xss -> (x::xs)::(cons_all x xss)
let cons_all (x:’a) (xss:(’a list) list) : (’a list) list =
List.map (fun xs -> x::xs) xss
(* Q2 *)
let rec branch_list (bt:’a btree) : (’a list) list =
match bt with
Empty -> [[]]
| Node(x,bt1,bt2) ->
(cons_all x ((branch_list bt1)@(branch_list bt2)))
(* Q3 *)
let rec is_branch (xs:’a list) (bt:’ btree) : bool =
match xs, bt with
[], Empty -> true
| x::xs, Node(y,bt1,bt2) ->
(x=y) && ((is_branch xs bt1) || (is_branch xs bt2))
| _ -> false
