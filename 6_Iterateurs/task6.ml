let apply_to_list (f : 'a -> 'b) (lst : 'a list) : 'b list =
List.map f lst;;

let apply_with_index (f : int -> 'a -> 'b) (lst : 'a list) : 'b list =
List.mapi f lst; 


let fold_left_acc (f : 'acc -> 'a -> 'acc) (acc : 'acc) (lst : 'a list) : 'acc =
List.fold_left f acc lst;; 


let fold_left (f : 'a -> 'acc -> 'acc) (lst : 'a list) (acc : 'acc) : 'acc =
List.fold_left (fun acc x -> f x acc) acc lst;; 


let filter (pred : 'a -> bool) (lst : 'a list) : 'a list =
List.filter pred lst;; 


let fold_left_acc_with_list (f : 'acc -> 'a -> 'acc * 'b) (acc : 'acc) (lst : 'a list) : 'acc * 'b list =
List.fold_left (fun (acc, res) x -> let acc', res' = f acc x in (acc', res :: res')) (acc, []) lst;;





