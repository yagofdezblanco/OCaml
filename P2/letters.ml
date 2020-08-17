let lowercase = function c ->
   let n = int_of_char c in
   if (n >= 65 && n <= 90) || (n >= 192 && n <= 214) || (n >= 216 && n <= 222)
   then char_of_int (n + 32)
   else c;;


let uppercase = function c ->
   let n = int_of_char c in
   if (n >= (65+32) && n <= (90+32)) || (n >= (192+32) && n <= (214+32)) || (n >= (216+32) && n <= (222+32))
   then char_of_int (n - 32)
   else c;;
