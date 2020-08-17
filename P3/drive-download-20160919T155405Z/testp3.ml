(*** Tests practica 3 Paradigmas Programación ***)

(*** Funciones Auxiliares ***)

let test f1 f2 x = 
	try f1 x = f2 x with  
		_ -> try f1 x; false with
		e1 -> try f2 x; false with
		e2 -> match e1,e2 with
			 Failure _, Failure _ -> true
			|Not_found, Not_found -> true
			|Invalid_argument _, Invalid_argument _ -> true
			|_ -> false;;

let check_test_list l function_name = if List.fold_left (&&) true l then print_endline (function_name^": PASSED")
				      else print_endline (function_name^": ERROR");;

(*** Listas de prueba ***)

let l1 = [3;5;1];;
let l2 = [5; 4; 3; 2];;
let l3 = [4;5;2;1;6;3;10];;
let l4 = ["ab";"cd";"ef"];;
let l5 = [3;2;1;5;3;2;1;0];;
let l6 = [3;7;1];;
let l7 = [2.0; 3.0; 4.0];;
let l8 = [3; 1];;

let ll1 = [[1;2;];[3;4]];;
let ll2 = [[4;1]; []; [-1;7]];;

(*** Tests hd ***)

let test_list_hd = [test Mylist.hd List.hd l1;
		    test Mylist.hd List.hd l2;
		    test Mylist.hd List.hd l3
		   ];;

check_test_list test_list_hd "hd";;

(*** Tests tl ***)

let test_list_tl = [test Mylist.tl List.tl l1;
		    test Mylist.tl List.tl l2;
		    test Mylist.tl List.tl l3
		   ];;

check_test_list test_list_tl "tl";;

(*** Tests length ***)

let test_list_length = [test Mylist.length List.length l1;
			test Mylist.length List.length l2;
			test Mylist.length List.length l3;
			test Mylist.length List.length []
		       ];;

check_test_list test_list_length "length";;

(*** Tests rev ***)

let test_list_rev = [test Mylist.rev List.rev l1;
		     test Mylist.rev List.rev l2;
		     test Mylist.rev List.rev l3;
		     test Mylist.rev List.rev []
		    ];;

check_test_list test_list_rev "rev";;

(*** Tests nth ***)

let test_list_nth = [test (Mylist.nth l1) (List.nth l1) 2;
		     test (Mylist.nth l2) (List.nth l2) 3
		    ];;

check_test_list test_list_nth "nth";;

(*** Tests append ***)

let test_list_append = [test (Mylist.append l1) (List.append l1) l3;
			test (Mylist.append l1) (List.append l1) [];
			test (Mylist.append l3) (List.append l3) l1
		       ];;

check_test_list test_list_append "append";;

(*** Tests rev_append ***)

let test_list_rev_append = [test (Mylist.rev_append l1) (List.rev_append l1) l3;
			    test (Mylist.rev_append l1) (List.rev_append l1) [];
			    test (Mylist.rev_append l3) (List.rev_append l3) l1
			   ];;

check_test_list test_list_rev_append "rev_append";;

(*** Tests concat ***)

let test_list_concat = [test Mylist.concat List.concat ll1;
			test Mylist.concat List.concat ll2;
			test Mylist.concat List.concat []
		       ];;

check_test_list test_list_concat "concat";;

(*** Tests flatten ***)

let test_list_flatten = [test Mylist.flatten List.flatten ll1;
			 test Mylist.flatten List.flatten ll2;
			 test Mylist.flatten List.flatten []
			];;

check_test_list test_list_flatten "flatten";;

(*** Tests map ***)

let test_list_map = [test (Mylist.map ((+) 3)) (List.map ((+) 3)) l3;
		     test (Mylist.map (( * ) 2)) (List.map (( * ) 2)) l2;
		     test (Mylist.map ((+) 3)) (List.map ((+) 3)) []
		    ];;

check_test_list test_list_map "map";;

(*** Tests map2 ***)

let test_list_map2 = [test (Mylist.map2 (+) l1) (List.map2 (+) l1) l6
		     ];;

check_test_list test_list_map2 "map2";;

(*** Tests fold_left ***)

let test_list_fold_left = [test (Mylist.fold_left (+) 0) (List.fold_left (+) 0) l1;
			   test (Mylist.fold_left (+) 5) (List.fold_left (+) 5) [];
			   test (Mylist.fold_left ( * ) 1) (List.fold_left ( * ) 1) l3;
			   test (Mylist.fold_left (^) "") (List.fold_left (^) "") l4;
			   test (Mylist.fold_left ( ** ) (List.hd l7)) (List.fold_left ( ** ) (List.hd l7)) (List.tl l7)
			  ];;

check_test_list test_list_fold_left "fold_left";;

(*** Tests fold_right ***)

let test_list_fold_right = [test (Mylist.fold_right (+) l1) (List.fold_right (+) l1) 0;
			    test (Mylist.fold_right (+) []) (List.fold_right (+) []) 5;
			    test (Mylist.fold_right ( * ) l3) (List.fold_right ( * ) l3) 1;
			    test (Mylist.fold_right (^) l4) (List.fold_right (^) l4) ""
			   ];;

check_test_list test_list_fold_right "fold_right";;

(*** Propiedades para las funciones que las usan ***)

let p1 x = x>=0;;
let p2 x = x mod 2 = 0;;
let p3 x = x>900000;;

(*** Tests for_all ***)

let test_list_for_all = [test (Mylist.for_all p1) (List.for_all p1) l1;
			 test (Mylist.for_all p1) (List.for_all p1) l3;
			 test (Mylist.for_all p2) (List.for_all p2) l1;
			 test (Mylist.for_all p2) (List.for_all p2) l3;
			 test (Mylist.for_all p2) (List.for_all p2) []
			];;

check_test_list test_list_for_all "for_all";;

(*** Tests find ***)

let test_list_find = [test (Mylist.find p1) (List.find p1) l1;
		      test (Mylist.find p1) (List.find p1) l3;
		      test (Mylist.find p2) (List.find p2) l3
		     ];;

check_test_list test_list_find "find";;

(*** Tests exists ***)

let test_list_exists = [test (Mylist.exists p1) (List.exists p1) l1;
			test (Mylist.exists p1) (List.exists p1) l3;
			test (Mylist.exists p2) (List.exists p2) l1;
			test (Mylist.exists p2) (List.exists p2) l3;
			test (Mylist.exists p2) (List.exists p2) []
		       ];;

check_test_list test_list_exists "exists";;

(*** Tests filter ***)

let test_list_filter = [test (Mylist.filter p1) (List.filter p1) l1;
			test (Mylist.filter p1) (List.filter p1) l3;
			test (Mylist.filter p2) (List.filter p2) l1;
			test (Mylist.filter p2) (List.filter p2) l3;
			test (Mylist.filter p2) (List.filter p2) []
		       ];;

check_test_list test_list_filter "filter";;

(*** Tests find_all ***)

let test_list_find_all = [test (Mylist.find_all p1) (List.find_all p1) l1;
			  test (Mylist.find_all p1) (List.find_all p1) l3;
			  test (Mylist.find_all p2) (List.find_all p2) l1;
			  test (Mylist.find_all p2) (List.find_all p2) l3;
			  test (Mylist.find_all p2) (List.find_all p2) []
			 ];;

check_test_list test_list_find_all "find_all";;

(*** Tests partition ***)

let test_list_partition = [test (Mylist.partition p1) (List.partition p1) l1;
			   test (Mylist.partition p1) (List.partition p1) l3;
			   test (Mylist.partition p2) (List.partition p2) l1;
			   test (Mylist.partition p2) (List.partition p2) l3;
			   test (Mylist.partition p2) (List.partition p2) []
			  ];;

check_test_list test_list_partition "partition";;

(*** Tests mem ***)

let test_list_mem = [test (Mylist.mem 2) (List.mem 2) l1;
		     test (Mylist.mem 5) (List.mem 5) l3;
		     test (Mylist.mem 7) (List.mem 7) []
		    ];;

check_test_list test_list_mem "mem";;

(*** Tests split ***)

let l1_split = [(1,'a');(3,'b');(2,'x');(-1,'g')];;
let l2_split = [("hola","hello");("adios","bye");("grande","big")];;

let test_list_split = [test (Mylist.split) (List.split) l1_split;
		       test (Mylist.split) (List.split) l2_split;
		       test (Mylist.split) (List.split) []
		      ];;

check_test_list test_list_split "split";;

(*** Tests combine ***)

let test_list_combine = [test (Mylist.combine l1) (List.combine l1) l6
			];;

check_test_list test_list_combine "combine";;

(*** Tests remove ***)

let test_list_remove = [Mylist.remove 3 l5 = [2;1;5;3;2;1;0];
			Mylist.remove 7 l5 = l5;
			Mylist.remove 5 [] = []
		       ];;

check_test_list test_list_remove "remove";;

(*** Tests remove_all ***)

let test_list_remove_all = [Mylist.remove_all 3 l5 = [2;1;5;2;1;0];
			    Mylist.remove_all 7 l5 = l5;
			    Mylist.remove_all 5 [] = []
			   ];;

check_test_list test_list_remove_all "remove_all";;

(*** Tests ldif ***)

let test_list_ldif = [Mylist.ldif l3 l1 = [4;2;6;10];
		      Mylist.ldif l3 l3 = []
		     ];;

check_test_list test_list_ldif "ldif";;

(*** Tests lprod ***)

let test_list_lprod = [Mylist.lprod l1 l4 = [(3,"ab");(3,"cd");(3,"ef");(5,"ab");(5,"cd");(5,"ef");(1,"ab");(1,"cd");(1,"ef")];
		       Mylist.lprod l8 l8 = [(3,3);(3,1);(1,3);(1,1)];
		       Mylist.lprod l1 [] = []
		      ];;

check_test_list test_list_lprod "lprod";;

(*** Tests divide ***)

let test_list_divide = [Mylist.divide l1 = ([3;1],[5]);
			Mylist.divide l3 = ([4;2;6;10],[5;1;3]);
			Mylist.divide [] = ([],[])
		       ];;

check_test_list test_list_divide "divide";;
