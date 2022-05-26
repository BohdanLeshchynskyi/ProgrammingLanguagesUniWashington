fun is_older(date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) = (#1 date2)
    then
	if (#2 date1) = (#2 date2) 
	then (#3 date1) < (#3 date2)
	else (#2 date1) < (#2 date2) 
    else (#1 date1) < (#1 date2)

fun date_in_month(date : int*int*int, month : int) =
    (#2 date) = month
			  
fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
	if date_in_month(hd(dates), month)
	then 1 + number_in_month(tl(dates), month)
	else number_in_month(tl(dates), month)
	
fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else
	number_in_month(dates, hd(months)) +
	number_in_months(dates, tl(months))
    
fun dates_in_month(dates: (int*int*int) list, month : int) =
    if null dates
    then []
    else
	if date_in_month(hd(dates), month)
	then hd(dates) :: dates_in_month(tl(dates), month)
	else dates_in_month(tl(dates), month)
			    
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else
	dates_in_month(dates, hd(months)) @
	dates_in_months(dates, tl(months))

fun get_nth(stringList : string list, n : int) =
    if null stringList
    then ""
    else
	if n = 1
	then hd(stringList)
	else get_nth(tl(stringList), n-1) 
       
fun date_to_string(date1 : int*int*int) =
    let val dates = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(dates, (#2 date1)) ^ Int.toString((#1 date1)) ^ ", "  ^ Int.toString((#3 date1))
    end

fun number_before_reaching_sum(sum : int,  posNumbers : int list) =
    let
	fun number_before_reaching_sum_2(sum : int,
					 counter: int,
					 posNumbers : int list) =
	    if (sum - hd(posNumbers)) <= 0
	    then counter
	    else number_before_reaching_sum_2(sum - hd(posNumbers), counter+1, tl(posNumbers))		   
    in
	number_before_reaching_sum_2(sum, 0, posNumbers)
    end

fun what_month(dayOfYear : int) =
    1 + number_before_reaching_sum(dayOfYear, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
    
fun month_range(day1 : int, day2 : int) =
    let
	fun range_month(month1, month2) =
	    if month1 = month2
	    then [month2]
	    else month1 :: range_month(month1 + 1, month2)
    in
	range_month(what_month(day1), what_month(day2))
    end

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
	let
	    fun oldest_nonempty(dates : (int*int*int) list) =
		if null (tl dates)
		then hd(dates)
		else let val tl_ans = oldest_nonempty(tl dates)
		     in
			 if is_older(hd(dates), tl_ans)
			 then hd(dates)
			 else tl_ans
		     end
	in
	    SOME(oldest_nonempty dates)
	end
	    

		   
