(* John Fischer, Coursera PL, Homework 2 *)

(* Simple function to compare two strings.  Will return true if the same string is provided *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

type name = {first : string,
	     middle : string,
	     last : string}
	     
(*(Problem 1a) This function takes a string and a string list.  It will return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it.  *)

fun all_except_option (test_string : string, xs : string list) =
    case xs of
	[] => NONE
      | x::xs' => if same_string(test_string,x)	then SOME(xs')
	else let val tail_contains = all_except_option(test_string,xs')
	     in
		 case tail_contains of
		     SOME(mod_list) => SOME(x::mod_list)
		   | NONE => NONE 
	     end
(*(Problem 1b) This function takes a list of a list of strings containing substitutions and a string, and returns a string list.  The resulting string list has all the strings that are in some list in substitutions that also has s *)
		 
fun get_substitutions1 (subs : (string list) list, test_string : string) =
    case subs of
	[] => []
      | x::xs' => case all_except_option(test_string, x) of
		      SOME(lst) => lst @ get_substitutions1(xs',test_string)
		    | NONE => get_substitutions1(xs',test_string)

(*(Problem 1c) This function is identical to get_substitutions1; however, it uses a tail-recursive local helper function *)
						
fun get_substitutions2 (subs : (string list) list, test_string : string) =
    let fun f (xs,result_acc) =
	    case xs of
		[] => result_acc
	      | x::xs' => case all_except_option(test_string,x) of
			      SOME(result) => f(xs',result_acc@result)
			    | NONE => f(xs',result_acc)				       
    in
	f(subs,[])
    end
	
(*(Problem 1d) This function takes a (string list) list of substitutions and a full name {first:string, middle:string, last:string} and returns a list of full names (name list).  The result is all of the full names you can produce by substituting for the first name (and only the first name using substitutions. *)

fun similar_names (subs : (string list) list, target_name : name) = 
    let
	val {first = f, middle = m, last = l} = target_name
	val alternates = get_substitutions2(subs,f)
	fun create_list (xs,result_acc) =
	    case xs of
		[] => target_name :: result_acc
	      | x::xs' =>  create_list(xs',{first = x, middle = m, last = l}::result_acc)
    in
	create_list(alternates,[])
    end

	
    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*(Problem 2a) This function takes a card and returns its color*)

fun card_color target_card =
    case target_card of (Clubs,_) => Black 
  | (Diamonds,_) => Red 
  | (Hearts,_) => Red 
  | (Spades,_) => Black

(*(Problem 2b) This function takes a card and returns its value. *)

fun card_value target_card =
    case target_card of (_,Ace) => 11
		      | (_,Num x) => x
		      | _ => 10

(*(Problem 2c) This function takes a list of cards, a card, and an exception.  It returns a list that has all of the cards from the original list except the card passed in the arguments.  If the card passed is not in the card list, an exception will be passed. *)				 
fun remove_card (card_list, target_card, e) =
    case card_list of
	[] => raise e 
     |  x::xs' => if x = target_card then xs'
		  else let val tl_list = remove_card(xs',target_card,e)
		       in
			   x::tl_list
		       end
			   
(*(Problem 2d) This function takes a list of cards and returns true if all the cards in the list are the same color.  *)

fun all_same_color (card_list) =
    case card_list of
	[] => true 
     | _::[] => true 
     | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))  

(*(Problem 2e) This function takes in a list of cards and returns the sum value of the list *)
				 
fun sum_cards (card_list) =
    let
	fun sum (xs,acc) =
	    case xs of
		[] => acc 
	      | x::xs' => sum(xs',acc+card_value(x))
    in
	sum(card_list,0)
    end
	
(*(Problem 2f) This function takes in a card list (hand) and an int (goal) and computes the score of the hand *)

fun score (hand, goal) =
    let val hand_value = sum_cards(hand)
    in
	case all_same_color(hand) of
	    true => if hand_value > goal then (3*(hand_value-goal)) div 2 else (goal - hand_value) div 2 
	  | false => if hand_value > goal then (3*(hand_value-goal)) else (goal - hand_value)
    end										    

(*(Problem 2g) This function takes a list of cards (deck), a list of moves, and an int (goal) and returns the score at the end of the game after processing some or all of the moves in the move list in order. *)
 	
fun officiate (deck, move_list, goal) =
    let
	fun turn (deck, hand, move_list) =
	    case move_list of
		[] => score(hand,goal)
	      | current_move::move_list' =>
		case current_move of
		    Discard c => turn(deck,remove_card(hand,c,IllegalMove),move_list')
		  | Draw => case deck of
				[] => score (hand,goal)
			     |  top_card::deck' => let val new_hand = top_card::hand in
						       if sum_cards(new_hand) > goal
						       then score(new_hand,goal)
						       else turn(deck',new_hand,move_list') 
						   end
    in
	turn(deck, [], move_list)
    end

(*
fun minimize_delta (hand_value,ace_count,goal) =
	    case hand_value - goal > 2 of
		true => if ace_count > 0 then minimize_delta(hand_value-10,ace_count-1,goal) else hand_value
	      | false => hand_value
*)

	
(*Challenge Problems *)

fun score_challenge (hand, goal, aces) =
    let
	val hand_value = sum_cards(hand)
	fun abs_val x =
	    case x < 0 of
		true => ~x
	      | false => x
	fun minimize_delta (hand_value,ace_count) =
	    case hand_value - goal > 2 of
		true => if ace_count > 0 then minimize_delta(hand_value-10,ace_count-1) else hand_value
	      | false => hand_value
    in
	case all_same_color(hand) of
	    true => if hand_value - (10*aces) > goal then abs_val((3*minimize_delta(hand_value,aces)-goal) div 2) else abs_val((goal - hand_value) div 2) 
	  | false => if hand_value - (10*aces) > goal then abs_val((3*(minimize_delta(hand_value,aces)-goal))) else abs_val(goal - hand_value)
    end										    

	
fun officiate_challenge (deck, move_list, goal) =
    let
	fun turn (deck, hand, move_list,aces) =
	    case move_list of
		[] => score_challenge(hand,goal,aces)
	      | current_move::move_list' =>
		case current_move of
		    Discard c => (
		     case c of
			 (_,Ace) => turn(deck,remove_card(hand,c,IllegalMove),move_list',aces-1)
		       | _ => turn (deck,remove_card(hand,c,IllegalMove),move_list',aces) )  
		  | Draw => case deck of
				[] => score_challenge(hand,goal,aces)
			     |  top_card::deck' =>
				let val new_hand = top_card::hand in
				    case card_value(top_card) = 11 of
					true => if sum_cards(new_hand) - ((aces+1) * 10) > goal then score_challenge(new_hand,goal,aces+1) else turn(deck',new_hand,move_list',aces+1)
				     | false => if sum_cards(new_hand) - ((aces+1) * 10) > goal then score_challenge(new_hand,goal,aces) else turn(deck',new_hand,move_list',aces)
				end
    in
	turn(deck, [], move_list,0)
    end

