(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x, 0)))

val longest_string1 = List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) ""

val longest_string2 = List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) ""

fun longest_string_helper f = List.foldl (fn (x, y) => if f (String.size(x), String.size(y)) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f xs = case xs of
    [] => raise NoAnswer
  | x::xs' => case f x of 
                  NONE => first_answer f xs'
                | SOME v => v

fun all_answers f xs = 
    let fun loop (xs, acc) = case xs of 
        [] => SOME acc
      | x::xs' => case f x of
                      NONE => NONE
                    | SOME lst => loop(xs', lst@acc)
    in loop(xs, [])
    end            

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (t, p) = g (fn () => 0) (fn s => if s = t then 1 else 0) p

fun check_pat p = 
    let 
        fun all_variable_strings (p, acc) = 
            case p of 
                Variable x => x::acc
              | TupleP ps => List.foldl (fn (p, a) => all_variable_strings (p,a)) acc ps
              | ConstructorP (_, p) => all_variable_strings(p, acc)
              | _ => acc
        fun no_repeat xs = 
            case xs of 
                [] => true
              | x::xs' => not(List.exists (fn v => v = x) xs') andalso no_repeat xs'
    in
        no_repeat(all_variable_strings(p, []))
    end

fun match (v, p) = case (v, p) of
    (_, Wildcard) => SOME []
  | (v, Variable s) => SOME [(s, v)]
  | (Unit, UnitP) => SOME []
  | (Const a, ConstP b) => if a = b then SOME [] else NONE
  | (Tuple vs, TupleP ps) => if length vs = length ps 
                             then all_answers match (ListPair.zip (vs, ps)) 
                             else NONE
  | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match (v, p) else NONE
  | _ => NONE

fun first_match v ps = SOME (first_answer (fn p => match (v, p)) ps) handle NoAnswer => NONE
