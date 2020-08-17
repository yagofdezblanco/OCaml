(*** Tests Práctica 5 Paradigmas Programación ***)

let gen_list f n =
  let rec aux acc = function
    0 -> acc
  | x -> aux ((f x n)::acc) (x-1)
in aux [] n;;

let long_rlist1 = gen_list (function _ -> function n -> Random.int 10) 1000000;;

let long_rlist2 = gen_list (function _ -> function n -> Random.int 10) 500000;;

let long_rlist3 = gen_list (function _ -> function n -> []) 500000;;

(**********)

let rec is_suml n l = match n,l with
     0, []   -> true
   | n, h::t -> is_suml (n-h) t
   | _       -> false;;

print_string "Testing suml..... ";;
flush stdout;;
if (is_suml (Mylist3.suml long_rlist1) long_rlist1)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let is_maxl m l = 
   List.for_all (function x -> x <=m) l && List.mem m l;;

print_string "Testing maxl..... ";;
flush stdout;;
if (is_maxl (Mylist3.maxl long_rlist1) long_rlist1)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let rec is_to0from n = function
     []   -> n < 0
   | h::t -> n=h && is_to0from (n-1) t;;

print_string "Testing to0from..... ";;
flush stdout;;
if (is_to0from 1000000 (Mylist3.to0from 1000000))
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let rec is_fromto m n = function 
    [] -> m > n
  | h::t -> h = m && is_fromto (m+1) n t;;

print_string "Testing fromto..... ";;
flush stdout;;
if (is_fromto 0 1000000 (Mylist3.fromto 0 1000000))
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let is_from1to n = is_fromto 1 n;;

print_string "Testing from1to..... ";;
flush stdout;;
if (is_from1to 1000000 (Mylist3.from1to 1000000))
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let rec is_append la l1 l2 = match la,l1,l2 with
     l, [], l'         -> l = l'
   | ha::ta, h1::t1, l -> ha = h1 && is_append ta t1 l
   | _                 -> false;;

print_string "Testing append..... ";;
flush stdout;;
if (is_append (Mylist3.append long_rlist1 long_rlist2) long_rlist1 long_rlist2)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)


let rec is_concat lc ll = match lc,ll with
     [], []             -> true
   | l, []::tl          -> is_concat l tl
   | hc::tc, (h::t)::tl -> hc = h && is_concat tc (t::tl)
   | _                  -> false;;

print_string "Testing concat (1)..... ";;
flush stdout;;
if (is_concat (Mylist3.concat [long_rlist1; []; [1;2;3]; long_rlist2])
                              [long_rlist1; []; [1;2;3]; long_rlist2])
then print_endline "passed"
else print_endline "ERROR!";;

print_string "Testing concat (2)..... ";;
flush stdout;;
if (is_concat (Mylist3.concat long_rlist3) long_rlist3)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let rec is_map lm f l = match lm,l with
     [], []       -> true
   | hm::tm, h::t -> f hm = f h && is_map tm f t
   | _            -> false;;

print_string "Testing map..... ";;
flush stdout;;
if (is_map (Mylist3.map (function x -> x) long_rlist1)
                        (function x -> x) long_rlist1)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let rec is_power p b e = 
   (p = 1 && e = 0) || (is_power (p/b) b (e-1));;

(*
let is_power p b e =
   float_of_int p = float_of_int b ** float_of_int e;;
*)

print_string "Testing power..... ";;
flush stdout;;
if (is_power (Mylist3.power 1 1000000) 1 1000000)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let is_fib fn n =
   let rec aux fia fi i =
      (fi = fn && i = n) || aux fi (fia+fi) (i+1)
   in
      aux 1 0 0
   ;;

print_string "Testing fib..... ";;
flush stdout;;
if (is_fib (Mylist3.fib 90) 90)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let f x =
   let rec aux a1 a2 = function
        (0,_)   -> a1, a2
      | (i1,i2) -> aux (a1 *. float i1) (a2 *. float i2) (i1-1, i2+1)
   in
      aux 1. 1. (x,1)
   ;;

print_string "Testing fact..... ";;
flush stdout;;
let f100_1, f100_2 = f 100 and mf = Mylist3.fact 100 and mfi = Mylist3.fact 1000000 in
if (mf = f100_1 || mf = f100_2) && (mfi = infinity)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

(* incseg long_rlist;;  WARNING *)

let is_incseg li l =
   let rec aux acc = function
        [],[] -> true
      | hi::ti, h::t -> let nacc = acc + h in
                        nacc = hi && aux nacc (ti,t)
      | _ -> false
   in
      aux 0 (li,l)
   ;;

print_string "Testing incseg..... ";;
flush stdout;;
if (is_incseg (Mylist3.incseg long_rlist1) long_rlist1)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let rec is_multicomp y lf x =
   let rec aux v = function
        []   -> y = v
      | f::tf -> aux (f x) tf
   in
      aux x (List.rev lf)
   ;;

let mapt f l = List.rev (List.fold_left (function al -> function b -> (f b)::al ) [] l);;
let lf = mapt (function x -> function _ -> x) long_rlist1;;

print_string "Testing multicomp..... ";;
flush stdout;;
if (is_multicomp (Mylist3.multicomp lf 0) lf 0)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

let rec is_sorted f = function
     [] | [_] -> true
   | h1::h2::t -> f h1 h2 && is_sorted f (h2::t);;

print_string "Testing insert..... ";;
flush stdout;;
let l = Mylist3.insert 999999 (Mylist3.from1to 1000000) in
if (is_sorted (<=) l) && (List.length l = 1000001)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

print_string "Testing insert_gen..... ";;
flush stdout;;
let l = Mylist3.insert_gen (<=) 999999 (Mylist3.from1to 1000000) in
if (is_sorted (<=) l) && (List.length l = 1000001)
then print_endline "passed"
else print_endline "ERROR!";;

(**********)

