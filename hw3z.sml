(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
fun only_capitals sl =
    List.filter (fn x => Char.isUpper (String.sub(x, 0))) sl;

(* 2 *)
fun longest_string1 sl =
    foldl (fn(x, y) => if String.size x > String.size y then x else y) "" sl;

(* 3 *)
fun longest_string2 sl =
    foldl (fn(x, y) => if String.size x >= String.size y
		       then x else y) "" sl;

(* 4.3 *)
fun longest_string_helper cmp sl =
    case sl of
	[] => ""
      | s::sl' => foldl (fn(x, acc) => if cmp(String.size s, String.size acc)
				       then x else acc) s sl';

(* 4.1 *)
val longest_string3 =
    longest_string_helper (fn(x, y) => x > y);

(* 4.2 *)
val longest_string4 =
    longest_string_helper (fn(x, y) => x >= y);


(* 5 *)
val longest_capitalized = longest_string1 o only_capitals;

(* 6 *)
val rev_string = String.implode o List.rev o String.explode;

(* 7 *)
fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs) =
    case f x of
	SOME v => v
      | NONE => first_answer f xs;

(* 8 *)
fun all_answers f lst =
    let
	fun helper acc [] = SOME acc
	  | helper acc (x::xs) =
	    case f x of
		NONE => NONE
	      | SOME v => helper (acc@v) xs
    in
	helper [] lst
    end;
			  
(* 9 *)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end;

(* 9.1 *)
val fun count_wildcards  =
    g (fn x => 1) (fn x => 0) ;

(* 9.2 *)
val count_wild_and_variable_lengths  =
    g (fn x => 1) (fn x => String.size x) ;

(* 9.3 *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p;

(* 10 *)
fun check_pat p =
    let
	fun verLst p =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (p, i) => (verLst p) @ i) [] ps
	      | ConstructorP(_,p) => verLst p
	      | _  => [];
	fun isRepeated l =
	    case l of
		[] => false
	      | x::xs =>(List.exists(fn y => x = y) xs) orelse (isRepeated xs)
    in
	not (isRepeated(verLst p))
    end;

(* 11 *)
fun match (v, p) =
    case (v,p) of
	(_ , Wildcard)            => SOME []
      | (_, Variable s)           => SOME [(s,v)]
      | (Unit, UnitP)             => SOME []
      | ((Const i1), (ConstP i2)) => if (i1 = i2) then SOME [] else NONE
      | ((Tuple vs),(TupleP ps))  => if (length vs = length ps)
				     then (all_answers match
						       (ListPair.zip(vs, ps)))
				     else NONE
      | (Constructor(s1, v),
	 ConstructorP(s2, p))     => if (s1 = s2)
				     then match(v, p)
				     else NONE
      | _ => NONE;

(* 12 *)
(*
fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs) =
    case f x of
	SOME v => v
      | NONE => first_answer f xs;
*)

fun first_match (v, ps) =
    let
	val result = first_answer (fn p => match(v, p)) ps
    in
	SOME result
	handle NoAnswer => NONE
    end;
(* 12 solution *)
(* fun first_match valu patlst =
    SOME (first_answer (fn pat => match (valu,pat)) patlst)
    handle NoAnswer => NONE *)
