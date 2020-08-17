let x = 1;;
(* val x: int = 1 *)
 
let y = 2;;
(* val y: int = 2 *)
 
x-y;;
(* - : int = -1 *)
 
let x = y in x - y;;
(* -:int = 0. *)
 
x - y;;
(* -: int = -1 *)
 
z;;
(* Error: Unbound value z
z no está definida*)

let z = x + y;;
(* val z: int = 3 *)
 
z;;
(* -: int = 3 
Ahora si está definida*)
 
let x = 5;;
(* val x: int = 5 *)
 
z;;
(* -: int = 3 *)
 
let y = 5 in x + y;;
(* -: int = 10 *)
 
x + y;;
(* - int = 7 *)
 
let p = 2,5;;
(*val p: int * int = (2,5)*)
 
snd p, fst p;;
(* -: int * int = (5,2) *)
 
p;;
(* -: int * int = (2,5) *)
 
let p = 0,1 in snd p, fst p;;
(* -: int * int = (1,0) *)
 
p;;
(* -: int * int = (2,5) *)

let x,y = p;;
(* val x: int = 2
val y: int = 5 *)
 
let z = x + y;;
(* val z: int = 7 *);;
 
let x,y = p,x;;
(*val x: int * int = (2,5)
val y: int = 2 *)
 
let x = let x,y = 2,3 in x * x + y;;
(* val x : int = 7 *)

x + y;;
(* - : int = 9 *)
 
z;;
(* -: int = 7 *)

let x = x + y in let y = x * y in x + y + z;;
(*- : int = 34 *)

x + y + z;;
(* - : int = 16 *)
 
int_of_float;;
(* -: float -> int = <fun> *)
 
float_of_int;;
(* -: int -> float = <fun> *)
 
int_of_char;;
(* -: char -> int = <fun> *)
 
char_of_int;;
(* -: int -> char = <fun> *)
 
abs;;
(* -: int -> int = <fun> *)
 
sqrt;;
(*-: float -> float = <fun> *)
 
truncate;;
(* -: float -> int = <fun> *)
 
ceil;;
(* -: float -> float = <fun> *)
 
floor;;
(* -: float -> float = <fun> *)
 
Char.code;;
(* -: char -> int = <fun> *)
 
String.length;;
(* -: string -> int = <fun> *)
 
fst;;
(* -: 'a * 'b -> 'a = <fun> 
 de una tupla de datos retorna un 
unico dato del mismo tipo (el primero)*)
 
snd;;
(* -: 'a * 'b -> 'b = <fun> 
Lo mismo que fst pero retorna el segundo dato*)
 
function x -> 2 * x;;
(* -: int -> int = <fun> *)
 
(function x -> 2 * x) (2 + 1);;
(* - : int = 6 *)

function (x,y) -> x;;
(*-: 'a * 'b -> 'b = <fun>*)
 
let f = function x -> 2 * x;;
(*val f: int -> int = <fun>*)
 
f (2+1);;
(* -: int = 6 *)
 
f 2 + 1;;
(* -: int = 5 *)

let n = 10;;
(* val n : int = 10 *)

let sum n = function x -> n + x;;
(* val sum : int -> int -> int = <fun> *)

sum 5;;
(* - : int -> int = <fun> *)

sum 1 2;;
(* - : int = 3 *)

let n = 1;;
(* val n : int = 1 *)

sum n 10;;
(* - : int = 11 *)

let sumn = sum n;;
(* val sumn : int -> int = <fun> 
sumn es igual a simar n + su argumento (en este caso 1) *)

sumn 100;;
(* - : int = 101 
sumn = sum n (que es 1) + 100= 101 *)

let n = 1000;;
(* val n : int = 1000 *)

sumn 100;;
(* - : int = 101 *)
