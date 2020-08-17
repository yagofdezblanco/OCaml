let suml l= 
    let rec faux sum = function
        [] -> sum
        | hd::tl -> faux (sum + hd) tl
    in faux 0 l;;

let maxl = function
     [] -> raise (Failure "maxl")
     | hd::tl -> List.fold_left max hd tl;;

let to0from n =
    let rec faux m l = 
	if m > n then []
	else if m = n then m::l 
	else faux (m+1) (m::l)
    in faux 0 [];;

let fromto m n = 
    let rec faux m l =
	if m > n then []
	else if m = n then List.rev(m::l) 
        else faux (m+1) (m::l)
    in faux m [];;

let from1to n =
    let rec faux m l = 
	if m > n then []
	else if m = n then List.rev(m::l) 
	else faux (m+1) (m::l)
    in faux 1 [];;

let append l1 l2 = 
    let rec faux l1 l2 = match l1 with 
        [] -> l2
        | hd::tl -> faux tl (hd::l2)
    in faux (List.rev l1) l2;;

let concat l =
    let rec faux l result = match l with
        [] -> result
        | hd::tl -> faux tl (append hd result)
    in faux (List.rev l) [];;

let map f l =
    let rec faux fn result = function
	[] -> List.rev result
        | hd::tl -> faux f ((f hd)::result) tl
    in faux f [] l;;

let power x y = 
    if y < 0 then invalid_arg "power"
    else let rec faux a b result = 
        if b = 0 then result
        else faux a (b-1) (a*result)
    in faux x y 1;; 

let fib n=
    if n < 0 then invalid_arg "fib"
    else let rec faux i a b = 
	if n=i then a
	else faux (i + 1) (a + b) a
   in faux 0 0 1;;

let fact n = 
    if n < 0 then invalid_arg "fact"
    else let rec faux num result =
             if num = 0 then result
             else faux (num-1) (float num *. result)
     in faux n 1.;;

let incseg l =
    let rec faux l result lr = match l with
        [] -> List.rev lr
      | hd::tl -> faux tl (result+hd) ((result+hd)::lr)
    in faux l 0 [];;

let multicomp l x =
    let rec faux l result = match l with
        [] -> result
        | hd::tl -> faux tl (hd result)
    in faux (List.rev l) x;;

let insert x l =
    let rec faux x l la ld = match l with
        [] -> append (List.rev la) ld
      | hd::tl -> if x <= hd then faux x [] la (append ld (hd::tl))
                else faux x tl (hd::la) ld
    in faux x l [] [x];;

let insert_gen f x l =
    let rec faux f x l lr = match l with
        [] -> lr
      | hd::tl -> if f x hd then faux f x [] (append (List.rev lr) (x::l))
                else faux f x tl (hd::lr)
    in faux f x l [];;
