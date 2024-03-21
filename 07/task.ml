module IntSet = Set.Make(struct
  type t = int
  let compare a b = a - b
end)

module StringSet = Set.Make(struct
  type t = string
  let compare a b = if a < b then (-1) else if a > b then 1 else 0
end)

let has_dupes l =
  let set = IntSet.of_list l in
  let len = IntSet.cardinal set in
  len <> List.length l;;

let uniq l =
  let set = IntSet.of_list l in
  IntSet.elements set

module CharMap = Map.Make(Char)
