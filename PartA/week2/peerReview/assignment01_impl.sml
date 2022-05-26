(* 01 *)
fun is_older(date1 : int * int * int, date2 : int * int * int) =
    let
	val yearEarlier = (#1 date1) < (#1 date2);
	val yearEqual = ((#1 date1) = (#1 date2));
	
	val monthEarlier = (#2 date1) < (#2 date2);
	val monthEqual = ((#2 date1) = (#2 date2));
	
	val dayEarlier = (#3 date1) < (#3 date2);
    in
	if yearEarlier orelse (yearEqual andalso monthEarlier) orelse (yearEqual andalso monthEqual andalso dayEarlier)
	then true
	else false
    end

(* 02 *)
fun number_in_month(dateList : (int * int * int) list, month : int) =
    if null dateList
    then 0
    else
	if ((#2 (hd dateList)) = month)
	then 1 + number_in_month(tl dateList, month)
	else number_in_month(tl dateList, month)

(* 03 *)
fun number_in_months(dateList : (int * int * int) list, monthList : int list) =
    if null monthList
    then 0
    else number_in_month(dateList, hd monthList) + number_in_months(dateList, tl monthList)

(* 04 *)
fun dates_in_month(dateList : (int * int * int) list, month : int) =
    if null dateList
    then []
    else
	if (#2 (hd dateList)) = month
	then
	    (hd dateList)::dates_in_month(tl dateList, month)
	else
	    dates_in_month(tl dateList, month)
	
(* 05 *)
fun dates_in_months(dateList : (int * int* int) list, monthList : int list) =
    if null monthList orelse null dateList
    then []
    else dates_in_month(dateList, hd monthList) @ dates_in_months(dateList, tl monthList)

(* 06 *)
fun get_nth(strList : string list, nth : int) =
    if nth = 1
    then hd strList
    else get_nth(tl strList, nth-1)

(* 07 *)
fun date_to_string(date : int * int * int) =
    let
	val monthNameList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
	val monthName = get_nth(monthNameList, (#2 date));
	val dateString = monthName ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date);
    in
	dateString
    end

(* 08 *)
fun number_before_reaching_sum(sum : int, intList : int list) =
    if sum <= (hd intList)
    then 0
    else
	if (null (tl intList)) orelse ((sum - hd (tl intList)) <= 0)
	then 1
	else 1 + number_before_reaching_sum(sum - hd intList, tl intList)
					   
    
(* 09 *)
fun what_month(dayInYear : int) =
    let
	val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31];
    in
	number_before_reaching_sum(dayInYear, days_in_months)+1
    end

(* 10 *)
fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	if day1 = day2
	then [what_month(day1)]
	else what_month(day1)::month_range(day1+1, day2)
    
(* 11 *)
fun oldest( datesList : (int * int * int) list) =
    if null datesList
    then NONE
    else
	if null (tl datesList)
	then SOME(hd datesList)
	else
	    if is_older(hd datesList, hd(tl datesList))
	    then
		if null (tl(tl datesList))
		then SOME(hd datesList)
		else oldest((hd datesList)::(tl(tl datesList)))
	    else
		if null (tl(tl datesList))
		then SOME(hd(tl datesList))
		else oldest((hd(tl datesList))::(tl(tl datesList)))
			   
					  
    
