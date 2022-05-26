(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2;

(* Check if list lst contains specified string str or not *)
fun contain_string (str, lst) =
    case lst of
	[]     => false
      | hd::tl => same_string(str, hd) orelse contain_string(str, tl);

(* put your solutions for problem 1 here *)
(* Problem 1a *)
fun all_except_option (str, str_list) =
    let fun all_except (str_list) =
	    case str_list of
		[]     => []
	      | hd::tl => if same_string(str, hd)
			  then tl
			  else hd::all_except(tl)
    in if contain_string(str, str_list)
       then SOME(all_except(str_list))
       else NONE
    end;

(* Problem 1b *)
fun get_substitutions1 (str_list_list, str) =
    case str_list_list of
	[]     => []
      | hd::tl => (case all_except_option(str, hd) of
		       NONE   => get_substitutions1(tl, str)
		     | SOME x => x @ get_substitutions1(tl, str));

(* Problem 1c *)
fun get_substitutions2 (str_list_list, str) =
    let fun tail_recurse (str_list_list, result) =
	    case str_list_list of
		[]     => result
	      | hd::tl => (case all_except_option(str, hd) of
			       NONE   => tail_recurse(tl, result)
			     | SOME x => tail_recurse(tl, result @ x))
    in tail_recurse(str_list_list, [])
    end;

(* Problem 1d *)			      
fun similar_names (str_list_list, {first=fst, middle=mid, last=lst}) =
    let val first_name_list = fst::get_substitutions2(str_list_list, fst)
	fun helper (name_list) =
	    case name_list of
	        []     => []
	      | hd::tl => {first=hd, middle=mid, last=lst}::helper(tl)
    in helper(first_name_list)
    end;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int; 
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw;

exception IllegalMove;

(* Helper function *)
fun same_card (c1 : card, c2 : card) =
    c1 = c2;

fun contain_card (c, cs) =
    case cs of
	[] => false
      | hd::tl => same_card(c, hd) orelse contain_card(c, tl);

(* put your solutions for problem 2 here *)
(* Problem 2a *)
fun card_color (card) =
    case card of
	(Clubs, _)    => Black
      | (Spades, _)   => Black
      | (Diamonds, _) => Red
      | (Hearts, _)   => Red;

(* Problem 2b *)
fun card_value (card) =
    case card of
	(_, Ace)   => 11
      | (_, Num x) => x
      | (_, _)     => 10; (* Rest of the rank (Jack, Queen, King) *)

(* Problem 2c *)
fun remove_card (cs, c, e) =
    let fun remove (cs) =
	    case cs of
		[]     => []
	      | hd::tl => if same_card(hd, c)
			  then tl
			  else hd::remove(tl)
    in if contain_card(c, cs)
       then remove(cs)
       else raise e
    end;

(* Problem 2d *)
fun all_same_color (cs) =
    case cs of
	[]         => true
      | _::[]      => true
      | c1::c2::tl => card_color(c1) = card_color(c2) andalso all_same_color(c2::tl);

(* Problem 2e *)
fun sum_cards (cs) =
    let fun sum (cs, result) =
	    case cs of
		[]     => result
	      | hd::tl => sum(tl, card_value(hd) + result)
    in sum(cs, 0)
    end;

(* Problem 2f *)
fun score (cs, goal) =
    let val sum_value = sum_cards(cs)
	val preliminary_score = if sum_value > goal
				then 3 * (sum_value - goal)
				else goal - sum_value
    in if all_same_color(cs)
       then preliminary_score div 2
       else preliminary_score
    end;

(* Problem 2g *)
fun officiate (cs, ms, goal) =
    let fun run_game (cs, ms, held_cards) =
	    case ms of
		[]              => score(held_cards, goal)
	      | Discard(c)::mtl => run_game(cs, mtl, remove_card(held_cards, c, IllegalMove))
	      | Draw::mtl       => case cs of
				       []      => score(held_cards, goal)
				     | c::ctl  => if (sum_cards(held_cards) + card_value(c)) > goal
						  then score(c::held_cards, goal)
						  else run_game(ctl, mtl, c::held_cards)
    in run_game(cs, ms, [])
    end;

(* This is wrong
fun score_challenge (cs, goal) =
    let fun count_aces (cs) = (* Count how many aces are there *)
	    case cs of
		[]           => 0
	      | (_, Ace)::tl => 1 + count_aces(tl)
	      | _::tl        => count_aces(tl)
	fun pre_score (value) = (* Compute preliminary score *)
	    if value > goal
	    then 3 * (value - goal)
	    else goal - value
	fun improve (curr, ace_count) = (* Improve the score *)
	    if ace_count > 0
	    then let val ace11 = pre_score(curr)
		     val ace1 = pre_score(curr - 10)
		 in
		     if ace11 < ace1 (* If ace11 is smaller than it's better so return that *)
		     then ace11
		     else improve(curr - 10, ace_count - 1) (* Else improve again with 11 turned into 1 *)
		 end
	    else pre_score(curr) (* If no more ace just compute the score *)
	val sum_value = sum_cards(cs)
	val ace_count = count_aces(cs)
	val preliminary_score = improve(sum_value, ace_count)
    in
	if all_same_color(cs)
        then preliminary_score div 2
        else preliminary_score
    end;

fun officiate_challenge (cs, ms, goal) =
    let fun run_game (cs, ms, held_cards) =
	    case ms of
		[]              => score_challenge(held_cards, goal)
	      | Discard(c)::mtl => run_game(cs, mtl, remove_card(held_cards, c, IllegalMove))
	      | Draw::mtl       => case cs of
				       []      => score_challenge(held_cards, goal)
				     | c::ctl  => if (sum_cards(held_cards) + card_value(c)) > goal
						  then score_challenge(c::held_cards, goal)
						  else run_game(ctl, mtl, c::held_cards)
    in run_game(cs, ms, [])
    end;
*)
