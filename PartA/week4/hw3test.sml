(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test1_2 = only_capitals ["mOtHer","Vater","bruDer","Schwester","Kater"] = ["Vater","Schwester","Kater"]

val test1_3 = only_capitals ["a","b","c"] = []										  	      
val test2_1 = longest_string1 ["A","bc","C"] = "bc"

val test2_2 = longest_string1 [] = ""

val test2_3 = longest_string1 ["AA","BB","c",""] = "AA" 
				       
val test3_1 = longest_string2 ["A","bc","C"] = "bc"

val test3_2 = longest_string2 [] = ""

val test3_3 = longest_string2 ["AA","BB","c",""] = "BB"

val test4a_1 = longest_string3 ["A","bc","C"] = "bc"

val test4a_2 = longest_string3 [] = ""

val test4a_3 = longest_string3 ["AA","BB","c",""] = "AA"

val test4b_1 = longest_string4 ["A","B","C"] = "C"

val test4b_2 = longest_string4 ["A","bc","C"] = "bc"

val test4b_3 = longest_string4 [] = ""

val test4b_4 = longest_string4 ["AA","BB","c",""] = "BB"

val test5_1 = longest_capitalized ["A","bc","C"] = "A"

val test5_2 = longest_capitalized ["a","bc","c"] = ""

val test5_3 = longest_capitalized ["AA","bB","c","CC"] = "AA"

val test6_1 = rev_string "abc" = "cba"

val test6_2 = rev_string "Uki" = "ikU"
				   
val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test7_2 = (first_answer (fn x => if x = 0 then SOME x else NONE) [1,2,3,4,5]) handle NoAnswer => 1

val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8_2 = all_answers (fn x => if x > 2 then SOME [x] else NONE) [3,4,5,6,7] = SOME [7,6,5,4,3]											 
val test8_3 = all_answers (fn x => if x > 2 then SOME [x] else NONE) [] = SOME []

val test9a_1 = count_wildcards Wildcard = 1

val test9a_2 = count_wildcards (ConstructorP("SOME", TupleP[Variable "Wildcard", Wildcard, Wildcard])) = 2 															    
val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1

val test9b_2 = count_wild_and_variable_lengths (TupleP[Wildcard, Variable "Uki", ConstructorP("SOME", TupleP[Variable "xxxYx", Wildcard, Wildcard])]) = 11
								     
val test9c_1 = count_some_var ("x", Variable("x")) = 1

val test9c_2 = count_some_var ("Uki", TupleP[Wildcard, Variable "Uki", ConstructorP("SOME", TupleP[Variable "Uki", Wildcard, Variable "Wildcard"])]) = 2
							 
val test10_1 = check_pat (Variable "x") = true

val test10_2 = check_pat (TupleP[Wildcard,ConstructorP("SOME",TupleP[Variable "y",Variable "z"]),Variable"x"]) = true					       
val test10_3 = check_pat (ConstructorP("QUATSCH", Wildcard)) = true

val test10_4 = check_pat (ConstructorP("SOMENONE", TupleP[Variable "x", Wildcard, UnitP, Variable "x", ConstructorP("SOME", TupleP[Variable "Wildcard", Wildcard])])) = false

val test11_1 = match (Const(1), UnitP) = NONE

(*val test11_2 = match (Tuple[Const(999),Constructor("SOME",Tuple[Tuple[Const(1),Const(2)],Tuple[Const(3),Const(4)]])], TupleP[Wildcard,ConstructorP("SOME",TupleP[TupleP[Variable "y",Variable "z"],Variable"x"])]) = SOME [("y",Const(1)),("z",Const(2)),("x",Tuple[Const(3),Const(4)])]*)
					     
val test12_1 = first_match Unit [UnitP] = SOME []

val test12_2 = first_match (Const(7)) [UnitP, TupleP[Variable"x", UnitP]] = NONE
