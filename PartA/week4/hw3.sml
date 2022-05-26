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

(*1*)
fun only_capitals strings =
    List.filter (fn s => (Char.isUpper o String.sub) (s,0)) strings

(*2*)		
fun longest_string1 strings =
    foldl (fn (s,acc) => if String.size s > String.size acc
			 then s
			 else acc) "" strings

(*3*)
fun longest_string2 strings =
    foldl (fn (s,acc) => if String.size s >= String.size acc
			 then s
			 else acc) "" strings
	  
(*4*)
fun longest_string_helper f strings =
    foldl (fn (s,acc) => if f(String.size s, String.size acc)
			 then s
			 else acc) "" strings

val longest_string3 = longest_string_helper op > (*(fn (a,b) => a > b)*)

val longest_string4 = longest_string_helper op >= (*(fn (a,b) => a >= b)*)
					    
(*5*)
val longest_capitalized =
    foldl (fn (s,acc) => if (Char.isUpper o String.sub) (s,0) andalso String.size s > String.size acc
			 then s
			 else acc) "" 

(*6*)
val rev_string = String.implode o List.rev o String.explode 

(*7*)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer 
      | x::xs' => case f x of
		      SOME v => v 
		    | NONE => first_answer f xs'

(*8*)
fun all_answers f xs =
    let fun helper (ls,acc) =
	    case ls of
		[] => SOME acc 
	      | x::ls' => case f x of
			      SOME v => helper (ls', v@acc) 
			    | NONE => NONE
    in
	helper (xs, [])
    end

(*9a*)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(*9b*)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)
			
(*9c*)
fun count_some_var (var, p) = g (fn _ => 0) (fn s => if s = var then 1 else 0) p

(*10*)
fun check_pat p =
    let fun collect_vars p =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (x,i) => collect_vars(x)@i) [] ps 
	      | ConstructorP (_,patt) => collect_vars patt
	      | _ => []
	fun check_duplicates vars =
	    case vars of
		[] => true 
	      | var::vars' => not (List.exists (fn s => s = var) vars') andalso check_duplicates vars'
    in
	(check_duplicates o collect_vars) p
    end

(*11*)
fun match (v,p) =
    case (v,p) of
	(_, Wildcard) => SOME [] 
      | (vl,Variable s) => SOME [(s,vl)] 
      | (Unit,UnitP) => SOME []
      | (Const i1,ConstP i2) => if i1 = i2
				then SOME []
				else NONE
      | (Tuple vs,TupleP ps) => if length vs = length ps
				then all_answers match (ListPair.zip(vs,ps))
				else NONE
      | (Constructor(s1,vl),ConstructorP(s2,pt)) => if s1 = s2
						    then match (vl,pt)
						    else NONE 
      | _ => NONE 

(*12*)
fun first_match vl ps =
    SOME(first_answer (fn p => match(vl,p)) ps) 
    handle NoAnswer => NONE
     	    

