type 'a queue = 'a list * 'a list

let empty = ([], [])

let is_empty q =
  match q with
  | ([], []) -> true
  | _ -> false

let snoc x (f, r) = (f, x :: r)

let hd q =
  match q with
  | ([], []) -> failwith "Empty queue"
  | (x :: _, _) -> x
  | ([], r) -> List.hd (List.rev r)

let tl q =
  match q with
  | ([], []) -> failwith "Empty queue"
  | (_ :: f, r) -> (f, r)
  | ([], r) -> ([], List.tl r)

let equal q1 q2 =
  let rec equal_lists l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> h1 = h2 && equal_lists t1 t2
    | _ -> false
  in
  let (f1, r1), (f2, r2) = q1, q2 in
  equal_lists (List.rev_append f1 r1) (List.rev_append f2 r2)

let of_list lst = (lst, [])

let to_list (f, r) = f @ List.rev r

let rev (f, r) = (r, f)

let append (f1, r1) (f2, r2) = (f1, r1 @ List.rev_append f2 r2)

let concat qq =
  let rec concat_aux acc qq =
    match qq with
    | [] -> acc
    | q :: qs -> concat_aux (append acc q) qs
  in
  concat_aux empty qq

let filter p (f, r) =
  let rec filter_aux acc = function
    | [] -> acc
    | x :: xs -> if p x then filter_aux (snoc x acc) xs else filter_aux acc xs
  in
  let filtered = filter_aux empty f in
  (to_list filtered, [])

let map f (f', r) =
  let rec map_aux acc = function
    | [] -> acc
    | x :: xs -> map_aux (snoc (f x) acc) xs
  in
  let mapped = map_aux empty f' in
  (to_list mapped, [])

let fold f init (f', r) =
  let rec fold_aux acc = function
    | [] -> acc
    | x :: xs -> fold_aux (f acc x) xs
  in
  fold_aux (fold_aux init (List.rev r)) f'








(* Fonction def *)
let def f x y z = if x = z then y else f z

(* Fonction sq *)
let sq x =
  let sq' = def (fun _ -> raise (Failure "undefined")) in
  if x <= 10 then x * x else sq' x

(* Fonction bogus_sq *)
let bogus_sq x =
  let sq' = def (fun _ -> 42) in
  sq' x

(* Fonction sq' *)
let sq' x =
  let assoc_list = [(i, i * i) | i <- 1 -- 10] in
  try List.assoc x assoc_list
  with Not_found -> raise (Failure "undefined")



