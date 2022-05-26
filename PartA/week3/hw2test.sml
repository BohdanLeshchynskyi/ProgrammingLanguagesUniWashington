(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2provided.sml";

val test1_1 = all_except_option ("string", ["string"]) = SOME [];

val test1_2 = all_except_option("E", ["A","B","C","E","D"]) = SOME  ["A","B","C","D"];

val test1_3 = all_except_option("e", ["A","B","C","E","D"]) = NONE;



val test2_1 = get_substitutions1 ([["foo"],["there"]], "foo") = [];

val test2_2 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"];

val test2_3  = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "A") = [];
								    

val test3_1 = get_substitutions2 ([["foo"],["there"]], "foo") = [];

val test3_2 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Freddie","F","Fredrick"];

val test3_3  = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "A") = [];



val test4_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="F", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}]

val test4_2 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Frida", middle="W", last="Smith"}) =
	      [{first="Frida", middle="W", last="Smith"}]

		  
val test5_1 = card_color (Clubs, Num 2) = Black

val test5_2 = card_color (Spades, Num 7) = Black

val test5_3 = card_color (Diamonds, Num 10) = Red

						  
val test6_1 = card_value (Clubs, Num 2) = 2

val test6_2 = card_value (Hearts, Num 10) = 10

val test6_3 = card_value (Clubs, Ace) = 11

val test6_4 = card_value (Spades, Queen) = 10


val test7_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test7_2 = remove_card ([(Clubs, Num 2), (Hearts, Num 10), (Clubs, Ace), (Spades, Queen)], (Clubs, Ace), IllegalMove) =
	      [(Clubs, Num 2), (Hearts, Num 10), (Spades, Queen)]

val test7_3 = remove_card ([(Clubs, Num 2), (Hearts, Num 10), (Clubs, Ace), (Clubs, Ace), (Spades, Queen)], (Clubs, Ace), IllegalMove) =
	      [(Clubs, Num 2), (Hearts, Num 10), (Clubs, Ace), (Spades, Queen)]
																     

val test8_1 = all_same_color [(Hearts, Ace), (Hearts, Num 10)] = true

val test8_2 = all_same_color [(Hearts, Ace), (Hearts, King), (Clubs, Num 7)] = false

val test8_3 = all_same_color [] = true

val test8_4 = all_same_color [(Spades, Ace)] = true 
										   

val test9_1 = sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13

val test9_2 = sum_cards [(Clubs, Num 2),(Clubs, Ace), (Hearts, King)] = 23

val test9_3 = sum_cards [] = 0
									    

val test10_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test10_2 = score ([(Hearts, Num 2),(Diamonds, Num 4),(Hearts, Queen)],10) = 9

val test10_3 = score ([], 10) = 5

val test10_4 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
								 

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

