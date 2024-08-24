(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, sl) =
    let fun helper(visited, []) = NONE 
	  | helper(visited, s::sl)  =
	    if same_string(str, s) then
		SOME (List.rev visited @ sl)
	    else helper(s::visited, sl)
    in
	helper([], sl)
    end;

fun get_substitutions1 (sll, s) =
    case sll of
	 [] => []
      |  sl::sll =>
	 case all_except_option (s, sl) of
	     NONE => get_substitutions1 (sll, s)
	  | SOME sl => sl @  get_substitutions1 (sll, s);

fun get_substitutions2 (sll, s) =
    let fun helper(sll, acc) =
       case sll of
	 [] => acc
      |  sl::sll =>
	 case all_except_option (s, sl) of
	     NONE => helper(sll, acc)
	   | SOME sl => helper (sll, sl @ acc)
    in
	helper(sll, [])
    end;

fun similar_names (sll, {first=x,middle=y,last=z}) =
    let val sub =  get_substitutions2 (sll, x)
	fun aux sub =
	     case sub of
		 [] => []
		 | s::sub' => {first=s,last=z,middle=y} :: aux sub'
    in
	 {first=x,middle=y,last=z} :: aux sub
    end;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (s, _) =
    case s of
	Clubs => Black
     |  Spades => Black
     | _ => Red;

fun card_value (_, r) =
    case r of
	Num i => i
     |  Ace => 11
     | _  => 10;

fun remove_card (cs, c, e) =
    let fun aux(acc, cs) =
    case cs of
	c1::rcs => if c1 = c
		   then acc @ rcs
		   else aux(acc @ [c1], rcs)
      | _ => raise e
    in
	aux([], cs)
    end;
fun all_same_color cs = 
	case cs of
	    [] => true
	  | [_] => true
	  | head::(neck::rest) => card_color head = card_color neck andalso
				  all_same_color (neck::rest);

fun sum_cards cs =
    let fun aux(sum, cs) =
	    case cs of
		[] => sum
	      | c::rcs => aux(sum + (card_value c), rcs)
    in
	aux(0, cs)
    end;

fun score (cl, g) =
    let val s = sum_cards cl
	val sc = if s > g then 3 * (s - g) else g - s
    in
	if (all_same_color cl) then (sc div 2) else sc	
    end;

fun officiate (cl, ml, g) =
    let
	fun aux(cl,hcs,ml) =
	    case (cl, hcs, ml) of
		 (_, _, []) => score(hcs, g)
	      | ([], hcs, Discard c::rml)  => 
		aux([], remove_card (hcs, c, IllegalMove), rml)
	      | ([], hcs, Draw::_) => score(hcs, g)
	      | (c::rcl, hcs,  Discard d::rml) => 
		 aux(c::rcl, remove_card (hcs, d, IllegalMove), rml)
	      | (c::rcl, hcs, Draw::rml) => if sum_cards (c::hcs) > g
					    then score(c::hcs, g)
					    else aux(rcl, c::hcs, rml)						   
    in
	aux(cl, [], ml)
    end;
	 
