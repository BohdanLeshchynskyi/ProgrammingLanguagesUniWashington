fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if date1 = date2 then false
    else if
	#1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2 then false
    else if
	#2 date1 < #2 date2
    then true
    else if #2 date1 > #2 date2 then false
    else
	#3 date1 < #3 date2
			 		
fun number_in_month (xs : (int*int*int) list, y : int) =
    if null xs
    then 0
    else if #2 (hd xs) = y
    then 1 + number_in_month((tl xs),y)
    else number_in_month((tl xs),y)
			
fun number_in_months (xs : (int*int*int) list, ys : int list) =
    if null ys
    then 0
    else number_in_month (xs, (hd ys)) + number_in_months (xs, (tl ys))
			 
fun dates_in_month (xs : (int*int*int) list, y : int) =
    if null xs
    then []
    else if #2 (hd xs) = y
    then hd xs::dates_in_month ((tl xs),y)
    else dates_in_month ((tl xs),y)

fun dates_in_months (xs : (int*int*int) list, ys : int list) =
    if null ys
    then []
    else dates_in_month (xs, (hd ys))@dates_in_months (xs,( tl ys))

fun get_nth (xs : string list, n : int) =
    if n=1 then hd xs
    else get_nth (tl xs, n-1)
		 
fun date_to_string (date : int*int*int) =
		   let val months = (["January", "February", "March", "April", "May", "June", 
		                  "July", "August", "September", "October", "November", "December"])
		   in get_nth (months, #2 date)^" " ^ (Int.toString(#3 date)) ^ ","^" " ^ (Int.toString (#1 date))
		   end

fun number_before_reaching_sum (sum : int, ints : int list) =
    if hd ints >= sum
    then 0
    else 1 + number_before_reaching_sum (sum - hd ints, tl ints)

fun what_month (day_of_year: int) =
    let val days_in_month_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum (day_of_year, days_in_month_list) + 1
    end

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range (day1 + 1, day2)

fun oldest (dates: (int*int*int) list) =
    if null dates
    then NONE
    else let fun oldest_helper (dates: (int*int*int) list) =
		 if null (tl dates) then hd dates
		 else let val tail_oldest = oldest_helper (tl dates)
		      in
			  if is_older (hd dates, tail_oldest)
			  then hd dates
			  else tail_oldest
		      end
	 in
	     SOME (oldest_helper (dates))
	 end
	     
    
    

		
	    
				    
				
		  
	      
		  
				  
	      
