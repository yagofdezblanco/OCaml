();;
(* No de vuelve nada.
   Corrección: - : unit = () *)

2 + 5 * 3;;
(* - : int = 17 *)

1.0;;
(* - : float = 1 
Corrección: - : float = 1. (con punto decimal)*)


(**1.0 * 2;;**)
1 * 2;;
(* characters 0-3: Error: This expression has type float but an expression was expected of type int

Error de compilación, la operación '*'
esta destinada a operar con enteros 

Ahora devolverá - : int = 2 *)


(**2 - 2.0;;**)
2 - 2;;
(* characters 5-6: Error: This expression has type float but an expression was expected of type int

Lo mismo que la anterior, la operación '-'
esta destinada a operar con enteros.

Ahora devolverá - : int = 0 *)

(**3.0 + 2.0;;**)
3 + 2;;
(* characters 0-3: Error: This expression has type float but an expression was expected of type int

Lo mismo que la anterior, la operación '-'
esta destinada a operar con enteros 

Ahora devolverá - : int = 5 *)



5 / 3;;
(* - : int = 1 (Aproximación entera ya que la operación es int -> int)*)


5 mod 3;;
(* - : int = 2 *)

3.0 *. 2.0 ** 3.0;;
(* - : float =  *)
(**
3.0 = float_of_int 3;;
(* - : boolean = true 
Corrección:  - : bool = true *)
**)

(**sqrt 4;;**)
sqrt 4.0;;
(* characters 5-6: Error: This expression has type float but an expression was expected of type int

sqrt es una operacion float -> float con lo cual el compilador da error si intentamos introducirle un int

Ahora devolverá: - : float = 2.*)

int_of_float 2.1 + int_of_float (-2.9);;
(* - : int = -1
Corrección: - : int = 0 
(Supuse que el paso a int sería por aproximación y no por truncado.)*)

truncate 2.1 + truncate (-2.9);;
(* - : int = 0 (2-2)*)

floor 2.1 + truncate (-2.9);;
(* - : int = 0 (2-2)*)

(**ceil 2.1 +. ceil -2.9;;**)
ceil 2.1 +. ceil (-2.9);;
(* - : float = 1.
Corrección: Error: This expression has type float -> float
       but an expression was expected of type int

Se debe a que el -2.9 debe ir entre comillas*)

'B';;
(* - : char = B
Corrección - : char = 'B'*)

int_of_char 'A';;
(* - : int = codigo ascii de 'A' (65 parece ser)*)

char_of_int 66;;
(* - : char = 'B'*)

Char.code 'B';;
(* - : int = 66*)

Char.chr 67;;
(* - : char = 'C'*)

'\067';;
(* - : char = 'C'*)



(*Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'Ñ');;*)

Char.uppercase 'a';;
(* - : char = 'A'*)

Char.lowercase 'O';;
(* - : char = 'o'*)

"this is a string";;
(* - : string = 'this is a string'*)

String-length "longitud";;
(* - : int = 8
Corrección: Error: Unbound constructor String*)

(**"1999" + "1";;**)
(* Error de tipo de dato, no se pueden sumar strings con el operador + 
Error: This expression has type string but an expression was expected of type int*)
1999 + 1;; (*Si quieres sumar los numeros de la cadena*)
"1999" ^ "1";; (*Si quieres concatenar las cadenas*)

"1999" ^ "1";;
(*- : string = "19991"*)

int_of_string "1999" + 1;;
(* - : int = 2000 *)

"\064\065";;
(*- : string = "'asci64'A"
Corrección: - : string = "@A"*)

string_of_int 010;;
(*- : string = "10"*)

not true;;
(*- : bool = false*)

true && false;;
(*- : bool = false*)

true || false;;
(*- : bool = true*)

(1 < 2) = false;;
(*- : bool = false (true no es igual a false)*)

"1" < "2";;
(*- : bool = true*)

2 < 12;;
(*- : bool = true*)

"2" < "12";;
(*- : bool = false (Supongo que se comparan los valores de las letras de las cadenas (2!>1)) *)

"uno" < "dos";;
(*- : bool = false (u!<d)*)

2,5;;
(* No sabria decir que devuelve
Corrección: - : int * int = (2, 5)*)

"hola", "adios";;
(*Deduciendo de la anterior diria que - : string * string = ('hola', 'adios')*)

0, 0.0;;
(*- : int * float = (0, 0.0)*)

fst ('a',0);;
(* - : char = 'a' (retorna el primer componente de la dupla)*)

snd (false, true);;
(* - : bool = true (retorna el segundo componente de la dupla)*)


(1,2,3);;
(*- : int * int * int = (1, 2, 3)*)
(1,2),3;;
(*- : (int * int) * int = ((1, 2), 3)*)

fst((1,2),3);;
(*- : int * int = (1, 2)*)

if 3 = 4 then 0 else 4;;
(*- : int = 4*)

if 3 = 4 then "0" else "4";;
(*- : String = '4'*)

if 3 = 4 then 0 else "4";;
(*- : String = '4'*)

(if 3 < 5 then 8 else 10) + 4;;
(*- : int = 12*)

let pi = 2.0 *. asin 1.0;;
(*Se definiría pi como float y su valor, no se la salida exacta
Corrección: val pi : float = 3.14159265358979312*)

sin (pi /. 2.);;
(*devuelve un float con el valor del seno del float resultante de dividir 
decimalmente el propio pi por 2.0
- : float = 1.*)
