
module CharMap = Map.Make(Char)

let char_digit n =
  if n >= 0 && n <= 9 then
    let char_code_0 = Char.code '0' in
    let digit_char = Char.chr (char_code_0 + n) in

    digit_char
  else
    failwith "Invalid digit"

(* Teston la fonction char_digit *)
let () =
  for i = 0 to 9 do
  
    Printf.printf "char_digit %d = %c\n" i (char_digit i)
  done
