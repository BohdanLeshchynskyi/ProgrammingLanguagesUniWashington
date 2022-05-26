val x = 2 + 5;
		
val y = x + 3;

val z = (x,y);

fun sumPair( pair:int*int ) =
    #1 pair + #2 pair

fun list_product(ls : int list) =
    if null ls
    then 1
    else hd ls * list_product(tl ls)

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else (hd xs) :: append(tl xs, ys)
			     
