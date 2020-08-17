let split l =
   let rec aux l1 l2 = function
    h1::h2::t -> aux (h1::l1) (h2::l2) t  
  | [] -> l1, l2 
  | [h] -> h::l1, l2
    in aux [] [] l;;

let merge la lb f = 
let rec aux result f l1 l2 = match l1, l2 with
    [], l | l, [] -> List.rev_append l result
  | h1::t1, h2::t2 -> if (f h1 h2) then aux (h1::result) f t1 l2
                      else aux (h2::result) f l1 t2
	in List.rev (aux [] f la lb);;

let rec lsort f = function
   [] -> []
  | [h] -> [h]
  | l -> let l1, l2 = split l in
         merge (lsort f l1) (lsort f l2) f;;

let asort f v = 
    let w = Array.copy v in
    let i1, i2, j = ref 0, ref 0, ref 0 in

    let merge f v i c d =
        i1 := i; i2 := c+1; j := i; 
        while (!i1 <= c) && (!i2 <= d) do
           if (f v.(!i1) v.(!i2))
           then begin w.(!j) <- v.(!i1); i1:=!i1+1 end
           else begin w.(!j) <- v.(!i2); i2:=!i2+1 end;
           j:=!j+1
        done;
        for l = !i1 to (c) do w.(!j) <- v.(l); j:=!j+1 done;
        for l = !i2 to d do w.(!j) <- v.(l); j:=!j+1 done;
        for l = i to d do v.(l) <- w.(l) done in

    let rec aux v i d =
       if i < d then
       begin
          let c = (i+d)/2 in
          begin
             aux v i c;
             aux v (c+1) d;
             merge f v i c d
          end
       end
    in aux v 0 (Array.length v - 1) ;;
