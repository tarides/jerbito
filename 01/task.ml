let char_succ =
if c < '\255' then 
 c |> int_of_char |> succ |> char_of_int
  else 
    '\000';;


let cr = "scream" in
let i_u_we = "I scream, you scream, we all " ^ cr in
let result = i_u_we ^ " for ice " in
result;;