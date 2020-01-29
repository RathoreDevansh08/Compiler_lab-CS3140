structure Term = struct

(* colours for different type of tokens *)
val rest = "\027[0m";
val blue = "\027[0;34m";        (* strings *)
val cyan = "\027[4;36m";        (* numericals *)
val green = "\027[0;32m";       (* identifiers *)
val red = "\027[0;31m";         (* operators *)
val yellow = "\027[0;33m";      (* keywords *)

fun highlight (KEYWORD x) = print(yellow ^ x ^ rest)
  | highlight (SYMBOL x) = print(red ^ x ^ rest)
  | highlight (COMMENT x) = print(blue ^ x ^ rest)
  | highlight (IDENTIFIER x) = print(green ^ x ^ rest)
  | highlight (NUMBER x) = print(cyan ^ Int.toString x ^ rest)

end
