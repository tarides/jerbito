let ajouter_element element liste =
element :: liste
let ma_liste = [1; 2; 3; 4]
let nouvelle_liste = ajouter_element 5 ma_liste;;



(* Création d'un tableau vide de taille 5 *)
let mon_tableau = Array.make 5 0;;

(* Accéder à un élément du tableau *)
let premier_element = mon_tableau.(0);;

(* Modifier un élément du tableau *)
mon_tableau.(1) <- 42;;

(* Accéder à la longueur du tableau *)
let taille = Array.length mon_tableau;;

(* Parcourir et afficher les éléments du tableau *)
for i = 0 to taille - 1 do
  Printf.printf "%d " mon_tableau.(i)
done;
Printf.printf "\n";;



let mon_tableau = [|1; 2; 3; 4; 5|];; (* Définition d'un tableau *)

mon_tableau.(0) <- 42;;  (* Remplacer le premier élément par 42 *)

(* Afficher le tableau après modification *)
Array.iter (fun x -> Printf.printf "%d " x) mon_tableau;;




let mon_tableau = [|1; 2; 3; 4; 5|];; (* Définition d'un tableau *)

mon_tableau.(0) <- 42;;  (* Remplacer le premier élément par 42 *)

(* Afficher le tableau après modification *)
Array.iter (fun x -> Printf.printf "%d " x) mon_tableau;;



let rec parcourir_liste = function
  | [] -> ()  (* Cas de base : liste vide, ne rien faire *)
  | tete :: queue ->  (* Cas récursif : tête de liste et reste de la liste *)
      (* Traiter la tête de liste *)
      Printf.printf "%d " tete;
      (* Appeler récursivement la fonction pour le reste de la liste *)
      parcourir_liste queue
;;

let ma_liste = [1; 2; 3; 4; 5];;
parcourir_liste ma_liste;;


let rec filtrage_liste = function
  | [] -> "Liste vide"
  | [x] -> "Liste avec un seul élément: " ^ string_of_int x
  | x :: y :: _ -> "Liste avec au moins deux éléments: premiers éléments sont " ^ string_of_int x ^ " et " ^ string_of_int y
;;



let rec somme_liste = function
  | [] -> 0  (* Cas de base : liste vide, la somme est 0 *)
  | tete :: queue -> tete + somme_liste queue  (* Cas récursif : addition de la tête avec la somme du reste de la liste *)
;;

let ma_liste = [1; 2; 3; 4; 5];;

let somme = somme_liste ma_liste;;

print_endline ("La somme des éléments de la liste est : " ^ string_of_int somme);;



(* Rechercher un element dans une liste *)

let rechercher_element element liste =
  let rec aux = function
    | [] -> false  (* Cas de base : liste vide, l'élément n'est pas trouvé *)
    | tete :: queue when tete = element -> true  (* L'élément est trouvé *)
    | _ :: queue -> aux queue  (* Recherche récursive dans le reste de la liste *)
  in
  aux liste
;;

let ma_liste = [1; 2; 3; 4; 5];;
let element_recherche = 3;;
let est_present = rechercher_element element_recherche ma_liste;;

if est_present then
  print_endline ("L'élément " ^ string_of_int element_recherche ^ " est présent dans la liste.")
else
  print_endline ("L'élément " ^ string_of_int element_recherche ^ " n'est pas présent dans la liste.");;



(* TRAVEAUX PRATIQUE *)

(* EXERCICE 1 :  *)

let last lst =
  match List.rev lst with
  | [] -> failwith "Liste vide"
  | x :: _ -> x
;;

(* Tests *)
let test1 = last [1; 2; 3; 4; 5] (* Doit renvoyer 5 *)
let test2 = last ["a"; "b"; "c"] (* Doit renvoyer "c" *)
let test3 = last [] (* Doit lever une exception "Liste vide" *)

(* Affichage des résultats des tests *)
let () =
  Printf.printf "Résultat du test 1 : %d\n" test1;
  Printf.printf "Résultat du test 2 : %s\n" test2;
  Printf.printf "Résultat du test 3 : Exception attendue\n"
;;


(* EXERCICE 2 :  *)

let swap lst =
  match lst with
  | [] | [_] -> lst 
  | x :: y :: reste -> y :: x :: reste  
;;

let test1 = swap [1; 2; 3] (* Doit renvoyer [2; 1; 3] *)
let test2 = swap [4; 5] (* Doit renvoyer [5; 4] *)
let test3 = swap [] (* Doit renvoyer [] *)

(* Affichage des résultats des tests *)
let () =
  Printf.printf "Résultat du test 1 : [%s]\n" (String.concat "; " (List.map string_of_int test1));
  
  Printf.printf "Résultat du test 2 : [%s]\n" (String.concat "; " (List.map string_of_int test2));
  
  Printf.printf "Résultat du test 3 : [%s]\n" (String.concat "; " (List.map string_of_int test3));
;;


(* EXERCICE 3 :  *)

let repeat element n =
  if n < 0 then
    []
  else
    List.init n (fun _ -> element)
;;

(* Tests *)
let test1 = repeat "Bonjour" 3 (* Doit renvoyer ["Bonjour"; "Bonjour"; "Bonjour"] *)
let test2 = repeat "Hi" (-2) (* Doit renvoyer [] *)

(* Affichage des résultats des tests *)
let () =
  Printf.printf "Résultat du test 1 : [%s]\n" (String.concat "; " test1);
  Printf.printf "Résultat du test 2 : [%s]\n" (String.concat "; " test2);
;;


(* EXERCICE 4:  *)
let range_i i j =
  if i > j then
    []
  else
    List.init (j - i + 1) (fun index -> i + index)
;;

(* Tests *)
let test1 = range_i 3 6 (* Doit renvoyer [3; 4; 5; 6] *)
let test2 = range_i 5 3 (* Doit renvoyer [] *)

(* Affichage des résultats des tests *)
let () =
  Printf.printf "Résultat du test 1 : [%s]\n" (String.concat "; " (List.map string_of_int test1));
  Printf.printf "Résultat du test 2 : [%s]\n" (String.concat "; " (List.map string_of_int test2));
;;
