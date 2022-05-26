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

(* 1 *)

(*  NOTE: the accumulator has to be set to a value : fun sum(l: int list) = List.foldl (fn (x, acc) => x + acc) 0 l *)
(*  fn: string list ->  string list *)
fun only_capitals (x : string list)   = List.filter( fn y => Char.isUpper(String.sub(y, 0))) x

(* 2 *)
fun longest_string1 (x : string list) = List.foldl (fn (y, acc) => if  (String.size y > String.size acc) then y else acc) "" x

(* 3 *)
fun longest_string2 (x : string list) = List.foldl (fn (y, acc) => if  (String.size y >= String.size acc) then y else acc) "" x

(* 4 *)
(*  fun longest_string_helper  has type (int * int -> bool)-> string list -> string
    ie:
    fn : int * int -> ( bool -> ( string list -> string )
*)
fun longest_string_helper f = List.foldl (fn (y, acc) => if f((String.size y),(String.size acc)) then y else acc) ""

val longest_string3  = longest_string_helper((fn (a, b) => (a > b)))

(* longest_string4 is the same as longest_string2 *)
val longest_string4  = longest_string_helper((fn (a, b) => (a >= b)))

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val  rev_string  = String.implode o List.rev o String.explode

(* 7 *)
(* first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried). *)

(* a map function version which has the same type as List.map
    val f = fn : ('a -> 'b) -> 'a list -> 'b list

fun f g xs =
    case xs of
        [] => []
           | x :: xs' => (g x)::((f g) xs')
*)

fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x :: xs' => case f x of
                       SOME v => v
                    | NONE  => first_answer f xs'

(* 8 *)
(* all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option (notice the 2 arguments are curried) *)

fun all_answers f xs =
    let fun h(acc, xs) =
            case xs of
                [] => SOME acc
              | x :: xs' => case f(x) of
                               SOME v => h((acc @ v), xs')
                             | NONE  => NONE
    in h([], xs)
end

(* 9 *)
fun count_wildcards p = g (fn x => 1) (fn y => 0) p

fun count_wild_and_variable_lengths p =  g (fn x => 1) (fn y => String.size y) p

fun count_some_var (s, p) = g (fn x => 0) (fn y => if (s = y) then 1 else 0) p;

(* 10 *)
fun list_helper p =
    case p of
        Wildcard          => []
      | Variable x        => x::[]
      | TupleP ps         => List.foldl (fn (q,i) => list_helper(q) @ i) [] ps
  	  | ConstructorP(s,p') => (s::[]; list_helper p')
	    | _                 => []

fun no_repeats xs =
    case xs of
        [] => true
     | x::xs' => if List.exists (fn y => y = x) xs' then false else no_repeats xs'

fun check_pat p = no_repeats (list_helper p)

(* 11 *)

fun match (v, p) =
    case (v, p) of
        (v, Wildcard) => SOME []
     | (Unit, UnitP ) => SOME []
     | (Const i, ConstP j) => if i = j then SOME [] else NONE
     | (Constructor(s, v'), Variable s') => SOME [(s, v')]
     | ( Constructor(s, sv), ConstructorP(s', sv')) => if s = s' then match(sv, sv') else NONE
     | (Tuple[], TupleP[]) => SOME []
     | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps) then all_answers match (ListPair.zip(vs, ps)) else NONE
     | (_, _) => NONE

(* 12 *)

fun first_match v ps = let val g = fn v => fn p => match(v,p) in
                           SOME( first_answer (g (v)) ps)
                           handle NoAnswer => NONE
                       end
