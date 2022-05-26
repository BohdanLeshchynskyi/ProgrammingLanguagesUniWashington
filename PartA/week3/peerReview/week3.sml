(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(target, str_list) =
    let fun helper(current, acc) = 
        case current of
            [] => NONE
            | str::str_listx => if same_string(str, target)
                                then SOME (acc@str_listx)
                                else helper(str_listx, acc@[str])
    in helper(str_list, []) end

fun get_substitutions1(str_list, target) =
    case str_list of
        [] => []
        | x::xs => 
            case all_except_option(target, x) of
                SOME y => y@get_substitutions1(xs, target)
                | _ => get_substitutions1(xs, target)

fun get_substitutions2(str_list, target) = 
    let
        fun helper(current, acc) =
            case current of
                [] => acc
                | x::xs => 
                    case all_except_option(target, x) of
                        SOME y => helper(xs, acc@y)
                        | _ => helper(xs, acc)
    in helper(str_list, []) end

fun similar_names(str_list, {first, middle, last}) = 
    let
        fun helper(lst) =
            case lst of
                [] => []
                | x::xs => {first=x, middle=middle, last=last}::helper(xs)
    in {first=first, middle=middle, last=last}::helper(get_substitutions2(str_list, first)) 
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

fun card_color(suit, rank) =
    case suit of
        Spades => Black
        | Clubs => Black
        | _ => Red

fun card_value(suit, rank) =
    case rank of
        Num x => x
        | Ace=> 11
        | _ => 10

fun remove_card(cs, c, e) =
    let fun helper(current, acc) = 
        case current of
            [] => raise e
            | card::card_listx => 
                                if c = card
                                then acc@card_listx
                                else helper(card_listx, acc@[card])
    in helper(cs, []) end

fun all_same_color(card_list) = 
    let fun helper(card_listx, color) =
        case card_listx of
            [] => true
            | x::xs =>  if card_color(x) = color
                        then helper(xs, color)
                        else false
    in case card_list of
        [] => true
        | x::xs => helper(xs, card_color x)
    end

fun sum_cards(card_list) =
    let fun helper(card_listx, acc) = 
        case card_listx of
            [] => acc
            | x::xs => helper(xs, acc+card_value(x))
    in helper(card_list, 0)
    end

fun score(card_list, goal) =
    let val sum = sum_cards(card_list)
    in
        if all_same_color(card_list)
        then 
            if sum > goal
            then ((sum - goal) * 3) div 2
            else (goal - sum) div 2
        else
            if sum > goal
            then (sum - goal) * 3
            else goal - sum
    end

fun officiate(card_list, move_list, goal) = 
    let fun helper(cl, ml, hcl) = 
        case ml of
            [] => score(hcl, goal)
            | x::xs => case x of
                        Discard y => helper(cl, xs, remove_card(hcl, y, IllegalMove)) 
                        | Draw => 
                                case cl of 
                                    [] => score(hcl, goal)
                                    | z::zs => if sum_cards(z::hcl) > goal
                                                then score(z::hcl, goal)
                                                else helper(remove_card(cl, z, IllegalMove), xs, z::hcl)
    in helper(card_list, move_list, [])
    end

fun score_challenge(card_list, goal) =
    let 
        fun get_all_aces(cl) =
            case cl of
                [] => 0
                | (suit, rank)::xs => case rank of
                                        Ace => 1 + get_all_aces(xs)
                                        | _ => get_all_aces(xs)
                                    
        val sum = sum_cards(card_list) - get_all_aces(card_list) * 10
    in
        if all_same_color(card_list)
        then 
            if sum > goal
            then ((sum - goal) * 3) div 2
            else (goal - sum) div 2
        else
            if sum > goal
            then (sum - goal) * 3
            else goal - sum
    end
