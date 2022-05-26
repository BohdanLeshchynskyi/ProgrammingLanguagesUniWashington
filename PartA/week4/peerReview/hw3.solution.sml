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
             | Datatype of string;

(**** you can put all your code here ****)

val S_size = String.size;

(* 1 *)
fun only_capitals strs =
    List.filter (fn str => Char.isUpper(String.sub(str, 0)))  strs;

(* 2 *)
fun longest_string1 strs =
    foldl (fn (s, best) => if S_size s > S_size best then s else best) "" strs;

(* 3 *)
fun longest_string2 strs =
    foldl (fn (s, best) => if S_size s >= S_size best then s else best) "" strs;

(* 4 *)
fun longest_string_helper pred strs =
    foldl (fn (s, best) => if pred(S_size s, S_size best) then s else best) "" strs;

val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

(* 5 *)
val longest_capitalized = longest_string3 o
                          (List.filter (fn s => Char.isUpper (String.sub(s, 0))))

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f x of
                     NONE => first_answer f xs'
                   | SOME v => v

(* 8 *)
fun all_answers f xs =
    let fun aux (ys, acc) =
            case ys of
                [] => SOME acc
              | (SOME v)::ys' => aux(ys', acc @ v)
              | (NONE)::_ => NONE
    in
        aux(List.map f xs, [])
    end;

(* 9a *)
fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

(* 9b *)
fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size x) p

(* 9c *)
fun count_some_var (s, p) =
    g (fn () => 0)
      (fn x => if s = x
             then 1
             else 0)
      p

(* 10 *)
fun check_pat pat =
    let
        fun get_vars p =
            case p of
                Variable x => [x]
              | TupleP ps => List.foldl (fn (p, acc) => acc @ get_vars p)
                                       []
                                       ps
              | ConstructorP (_, p) => get_vars p
              | _ => []
        fun has_duplicates xs =
            case xs of
                [] => false
              | x::xs' => (List.exists (fn x' => x' = x)  xs') orelse has_duplicates xs'
    in
        (not o has_duplicates o get_vars) pat
    end;

(* 11 *)
fun match (value, pattern) =
    case (value, pattern) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const v, ConstP c) => if v = c then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs <> List.length ps
                                then NONE
                                else all_answers match (ListPair.zip (vs, ps))
      | (Constructor (sv, v), ConstructorP (sp, p)) => if sv <> sp
                                                      then NONE
                                                      else match (v, p)
      | _ => NONE;

(* 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE
