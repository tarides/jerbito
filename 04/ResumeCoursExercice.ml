type subject = Math | English | History | Sport
type grade = int
type report = (subject * grade list) list

exception Subject_not_found
exception No_grade of subject

let add_subject subj rpt =
  if List.mem_assoc subj rpt then rpt
  else (subj, []) :: rpt

let add_grade subj grd rpt =
  try
    let grades = List.assoc subj rpt in
    (subj, grd :: grades) :: List.remove_assoc subj rpt
  with Not_found -> raise Subject_not_found

let get_grades subj rpt =
  try
    List.assoc subj rpt
  with Not_found -> raise Subject_not_found

let rec get_best_grade subj rpt =
  match get_grades subj rpt with
  | [] -> raise (No_grade subj)
  | hd :: tl -> List.fold_left max hd tl

let get_best_grade_opt subj rpt =
  try
    Some (get_best_grade subj rpt)
  with
  | Subject_not_found | No_grade _ -> None

let change_grades f subj rpt =
  let apply_f grades = List.map f grades in
  let updated = try apply_f (get_grades subj rpt) with Not_found -> raise Subject_not_found in
  (subj, updated) :: List.remove_assoc subj rpt

let compute_avg subj rpt =
  let grades = get_grades subj rpt in
  let sum = List.fold_left (+) 0 grades in
  let count = List.length grades in
  if count = 0 then 0.0
  else float_of_int sum /. float_of_int count

let get_best_grades_in subjects rpt =
  let best_grades_in_subject subj =
    try (subj, get_best_grade subj rpt)
    with No_grade _ -> raise (No_grade subj)
  in
  List.map best_grades_in_subject subjects





type subject = Math | English | History | Sport
type grade = int
type report = (subject * grade list) list

exception Subject_not_found
exception No_grade of subject

let add_subject subj rpt =
  if List.mem_assoc subj rpt then rpt
  else (subj, []) :: rpt

let add_grade subj grd rpt =
  try
    let grades = List.assoc subj rpt in
    (subj, grd :: grades) :: List.remove_assoc subj rpt
  with Not_found -> raise Subject_not_found

let get_grades subj rpt =
  try
    List.assoc subj rpt
  with Not_found -> raise Subject_not_found

let rec get_best_grade subj rpt =
  match get_grades subj rpt with
  | [] -> raise (No_grade subj)
  | hd :: tl -> List.fold_left max hd tl

let get_best_grade_opt subj rpt =
  try
    Some (get_best_grade subj rpt)
  with
  | Subject_not_found | No_grade _ -> None

let change_grades f subj rpt =
  let apply_f grades = List.map f grades in
  let updated = try apply_f (get_grades subj rpt) with Not_found -> raise Subject_not_found in
  (subj, updated) :: List.remove_assoc subj rpt

let compute_avg subj rpt =
  let grades = get_grades subj rpt in
  let sum = List.fold_left (+) 0 grades in
  let count = List.length grades in
  if count = 0 then 0.0
  else float_of_int sum /. float_of_int count

let get_best_grades_in subjects rpt =
  let best_grades_in_subject subj =
    try (subj, get_best_grade subj rpt)
    with No_grade _ -> raise (No_grade subj)
  in
  List.map best_grades_in_subject subjects













