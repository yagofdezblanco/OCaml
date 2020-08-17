let hd = function
	h::_ -> h
	| []->raise(Failure "hd");;

let tl = function
	_::t -> t
	| []->raise(Failure "tl");;

let rec length = function
	[] -> 0
	| _::t -> 1 + length t;;
	
let rec nth l n = 
	if n < 0 then invalid_arg "nth" else match l with 
	[] -> raise (Failure "nth")
	| h::t -> if n = 0 then h else nth t (n-1);;

let rec append l1 l2 = match l1 with
	[] -> l2
	| h::t -> h::append t l2;;

let rec rev = function
	[] -> []
        | h::t -> append (rev t) [h];;

let rec rev_append l1 l2 = match l1 with
    	[] -> l2
  	| h :: t -> rev_append t (h :: l2);;

let rec concat l1l2 = match l1l2 with
	[] -> []
        | l1::ln -> append l1 (concat ln);;

let flatten = concat;;

let rec map f = function
    	[] -> []
  	| h::t -> let r = f h in r :: map f t;;

let rec map2 f l1 l2 = match (l1,l2) with
    	([],[]) -> []
  	| (h1::t1),(h2::t2) -> let h = f h1 h2 in h::(map2 f t1 t2)
  	| (_, _) -> invalid_arg "different lengths";;

let rec fold_left f a l = match l with
  	| [] -> a
  	| h::t -> fold_left f (f a h) t;;

let rec fold_right f l a = match l with
  	| [] -> a
  	| h::t -> f h (fold_right f t a);;

let rec find p l = match l with
      	[] -> raise Not_found
    	| h::t -> if p h then h else find p t;;

let rec for_all p l = match l with
	[] -> true
      	| h::t -> p h && for_all p t;;

let rec exists p l = match l with
	[] -> false
      	| h::t -> p h || exists p t;;

let rec mem a l = match l with
	[] -> false
      	| h::t -> h = a || mem a t;;

let rec filter p = function
	[] -> []
      	| h::t -> if p h then h::filter p t else filter p t;;

let find_all = filter;;

let rec partition p = function
	[] -> ([],[])
      	| h::t -> let (ltrue, lfalse) = partition p t 
	in if p h then (h::ltrue, lfalse) else (ltrue, h::lfalse);;

let rec split = function
	[] -> ([], [])
	| (a,b)::l -> let (ra, rb) =
        split l in (a::ra, b::rb);;

let rec combine l1 l2 = match (l1,l2) with
	[], [] -> []
	| (h1::t1, h2::t2) -> (h1, h2)::(combine t1 t2) 
	| (_, _) -> invalid_arg "different lengths";;

let rec remove a = function
	[] -> []
	| h::t -> if h = a then t else h::(remove a t);;

let rec remove_all a = function
	[] -> []
       | h::t -> if a=h then remove_all a t else h::remove_all a t;;

let rec ldif l1 l2 = match (l1,l2) with
	[],_ -> []
	| l1, [] -> l1
	| l1,hl2::tl2 -> ldif (remove_all hl2 l1) tl2;;

let rec lprod_aux n l1 = match l1 with
	[] -> []
	| h::t -> (n,h)::(lprod_aux n t);;

let rec lprod l1 l2 = match l1 with 
	[] -> []
        | h1::t1 -> (lprod_aux h1 l2)@(lprod t1 l2);;

let rec divide = function
	h1::h2::t -> let t1, t2 = divide t in 
	h1::t1, h2::t2
        | l -> l,[];;




