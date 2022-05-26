fun alternate (ls: int list) =
    if null ls
    then 0
    else
	hd ls + (~1)*(alternate(tl ls))

			 
fun min_max (ls: int list) =
    if null ls
    then (0,0)
    else
	let fun max (some_ls: int list) =
		if null(tl some_ls)
		then hd some_ls
		else let val max_val = max(tl some_ls) in
			 if hd some_ls > max_val
			 then hd
			 else max_val
		     end
	    fun min (some_ls: int list) =
		if null(tl some_ls)
		then hd some_ls
		else let val min_val = min(tl some_ls) in
			 if hd some_ls < min_val
			 then hd some_ls
			 else min_val
		     end
	in
	    (min ls, max ls)
	end


