(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, ls) =
    case ls of
	[] => NONE 
      | strLs::tail => if same_string(strLs, str)
		       then SOME (tail)
		       else case all_except_option (str, tail) of
				NONE => NONE 
			      | SOME someLs => SOME (strLs::someLs)

fun get_substitutions1 (subst, s) =
    case subst of
	[] => []
      | list::tail => case all_except_option(s, list) of
			NONE => get_substitutions1(tail, s) 
		       | SOME ls => ls @ get_substitutions1(tail, s)

fun get_substitutions2 (subst, s) =
    let fun aux_tailRec(subst, s, acc) =
	    case subst of
		[] => acc 
	      | list::tail => case all_except_option(s, list) of
				  NONE => aux_tailRec(tail, s, acc) 
				| SOME ls => aux_tailRec(tail, s, ls@acc)
    in
	aux_tailRec(subst, s, [])
    end

fun similar_names (subst, {first=first, middle=middle, last=last}) =
    let fun add_to_names (first_names, acc) =
	    case first_names of
		[] => acc 
	      | fName::tail => add_to_names(tail, [{first = fName, middle = middle, last = last}]@acc)
    in
	[{first=first, middle=middle, last=last}] @ add_to_names(get_substitutions2(subst, first), [])
    end
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
	      
(* put your solutions for problem 2 here *)
	      
fun card_color (some_card) =
    case some_card of
	(Clubs,_) => Black 
      | (Spades,_) => Black 
      | (_,_) => Red 
	
fun card_value (some_card) =
    case some_card of
	(_, Num num) => num 
      | (_,Ace) => 11 
      | (_,_) => 10 

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | head::tail => if head = c
		      then tail
		      else head::remove_card(tail, c, e)

fun all_same_color (cs) =
    case cs of
	[] => true 
      | _::[] => true 
      | head::(neck::tail) => (card_color(head) = card_color(neck)) andalso all_same_color(neck::tail)
		     
fun sum_cards (cs) =
    let fun helper (cs, acc) =
	    case cs of
		[] => acc 
	      | head::tail => helper(tail, acc+card_value(head))
    in
	helper (cs, 0)
    end

fun score (cs, goal) =
    let val sum = sum_cards(cs)
	val same_color = all_same_color(cs)
    in
	if sum > goal
	then if same_color
	     then 3*(sum-goal) div 2
	     else 3*(sum-goal)
	else if same_color
	     then (goal-sum) div 2
	     else goal-sum
    end 

fun officiate (cs, moves, goal) =
    let fun current_state (cs, moves, held_cards) =
	    case (cs, moves, held_cards) of
		(cs, Discard(dis_card)::tail_moves, held_cards) => current_state(cs, tail_moves, remove_card(held_cards, dis_card, IllegalMove))
	      | (some_card::tail_cards, Draw::tail_moves, held_cards) => if sum_cards(some_card::held_cards) > goal
									 then score(some_card::held_cards, goal)
									 else current_state(tail_cards, tail_moves, some_card::held_cards) 
	      | ([], Draw::_, held_cards) => score(held_cards, goal)
	      | (_, [], held_cards) => score(held_cards, goal)
    in
	current_state(cs, moves, [])
    end
