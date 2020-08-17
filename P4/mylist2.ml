let hd = function
	h::_ -> h
	|[]->raise(Failure "hd");;

let tl = function
	_::t -> t
	|[]->raise(Failure "tl");;

let rec length_aux len = function
    [] -> len
  | h::t -> length_aux (len + 1) t

let length l = length_aux 0 l

let nth l n =
	if n < 0 then invalid_arg "nth" else
	let rec nth_aux l n =
	  match l with
	  | [] -> raise (Failure "nth")
	  | h::t -> if n = 0 then h else nth_aux t (n-1)
	in nth_aux l n;;

let rec append l1 l2 = match l1 with
	[] -> l2
	| h::t -> h::append t l2;;

let rec rev_append l1 l2 = match l1 with
    	[] -> l2
  	| h :: t -> rev_append t (h :: l2);;

let rev l = rev_append l [];;

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
        | (h::t) -> if p(h) then h else find p t;;

let rec for_all p l = match l with
	[] -> true
        | h::t -> p(h) && for_all p t;;

let rec exists p l = match l with
	[] -> false
      	| h::t -> p h || exists p t;;

let rec mem a l = match l with
	[] -> false
      	| h::t -> h = a || mem a t;;

let filter p =
        let rec aux l1 l2 = match l2 with
        [] -> rev l1
        | h::t -> if p h  then aux (h::l1) t else aux l1 t 
        in aux [];;

let find_all = filter;;

let partition p l =
	  let rec part_aux yes no = function
	  | [] -> (rev yes, rev no)
	  | x :: l -> if p x then part_aux (x :: yes) no l 
		else part_aux yes (x :: no) l in
	  part_aux [] [] l

let rec split = function
	[] -> ([], [])
	| (a,b)::l -> let (ra, rb) =
        split l in (a::ra, b::rb);;

let rec combine l1 l2 = match (l1,l2) with
	[], [] -> []
	| (h1::t1, h2::t2) -> (h1, h2)::(combine t1 t2) 
	| (_, _) -> invalid_arg "different lengths";;

let remove v l = 
	let rec remove_aux l1 = function
	    [] -> rev l1
            | h::t -> if v=h then (rev_append l1 t) else remove_aux (h::l1) t
	    in remove_aux [] l;;


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
