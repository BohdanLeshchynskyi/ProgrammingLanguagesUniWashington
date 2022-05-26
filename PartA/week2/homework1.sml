fun is_older( date1: int*int*int, date2: int*int*int ) =
    #1 date1 < #1 date2 orelse (*first most obvious case, then the rest of what could evaluate to "true"*)
    (#1 date1 = #1 date2 andalso (#2 date1 < #2 date2 orelse ( #2 date1 = #2 date2 andalso  #3 date1 < #3 date2 )))
		      
fun number_in_month(dates: (int*int*int)list, month: int) =
    if null dates
    then 0
    else
	if #2(hd dates) = month
	then number_in_month(tl dates, month) + 1
	else number_in_month(tl dates, month)

fun number_in_months(dates: (int*int*int)list, months: int list) =
    if null months
    then 0
    else
	 number_in_month(dates, hd months) + number_in_months(dates, tl months)
		    
fun dates_in_month(dates: (int*int*int)list, month: int) =
    if null dates
    then []
    else
	if #2(hd dates) = month
	then hd dates::dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int*int*int)list, months: int list) =
    if null months
    then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) =
    if n = 1
    then hd strings
    else
	get_nth(tl strings, n-1)

fun date_to_string(date: int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString (#3 date)^ ", " ^ Int.toString (#1 date)
    end

fun number_before_reaching_sum(sum: int, ls: int list) =
    let fun count(subsum: int, n: int, ls_ally: int list ) =
	    if subsum >= sum
	    then n - 1
	    else
		count(subsum + hd ls_ally, n + 1, tl ls_ally)
    in
 	count(0,0,ls)
    end
 
fun what_month(day: int) =
    let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, months) + 1
    end
	
fun month_range(day1: int, day2: int) =
    if day1>day2
    then []
    else
	what_month day1 :: month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else
	let fun find_oldest(nonempty_dates: (int*int*int) list) =
		if null (tl nonempty_dates)
		then hd nonempty_dates
		else let val tl_oldest = find_oldest(tl nonempty_dates)
		     in
			 if is_older(hd nonempty_dates, tl_oldest)
			 then hd nonempty_dates
			 else tl_oldest
		     end
	in
	    SOME(find_oldest dates)
	end

fun remove_multiples(ls: int list) =
    if null ls
    then []
    else
	let fun is_inList(some_ls: int list, to_check: int) = 
		if null some_ls
		then false
		else
		    if hd some_ls = to_check
		    then true
		    else is_inList(tl some_ls, to_check)
	    fun write_uniques(some_ls: int list) = 
		if null some_ls
		then []
		else
		    if is_inList(tl some_ls, hd some_ls)
		    then write_uniques(tl some_ls)
		    else hd some_ls :: write_uniques(tl some_ls)
	in
	    write_uniques(ls)
	end	    						    
	    
fun number_in_months_challenge(dates: (int*int*int)list, months: int list) =
    let val uniques = remove_multiples(months)
    in
	number_in_months(dates, uniques)
    end

fun dates_in_months_challenge(dates: (int*int*int)list, months: int list) =
    let val uniques = remove_multiples(months)
    in
	dates_in_months(dates, uniques)
    end
