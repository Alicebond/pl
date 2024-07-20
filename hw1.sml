(* year month day: int*int*int *)
(* date is a tuple with three int
val date1 = (2018, 10, 5) means 2018 October 5th
 *)

(* 1. *)
fun is_older (d1 : int*int*int, d2 : int*int*int) =
    (#1 d1 < #1 d2)
    orelse ((#1 d1 = #1 d2) andalso (#2 d1 < #2 d2))
    orelse ((#1 d1 = #1 d2) andalso (#2 d1 =  #2 d2)
	     andalso (#3 d1 < #3 d2))

(* 2. *)
fun number_in_month (ds : (int*int*int) list, m : int) =
    if null ds
    then 0
    else if (#2 (hd ds)) = m
    then 1 + number_in_month (tl ds, m)
    else number_in_month(tl ds, m);
	
(* 3. *)
fun number_in_months (ds : (int*int*int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month (ds, hd ms) + number_in_months(ds, tl ms);

(* 4. *)
fun dates_in_month (ds : (int*int*int) list, m : int) =
    if null ds
    then []
    else if(#2 (hd ds)) = m
    then (hd ds)::dates_in_month(tl ds, m)
    else dates_in_month(tl ds, m);

(* 5. *)
fun dates_in_months (ds : (int*int*int) list, ms : int list) =
    if null ms
    then []
    else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms);

(* 6. *)
fun get_nth (strs : string list, n : int) =
    if n = 1
    then hd strs
    else get_nth(tl strs, n - 1);

(* 7. *)
fun date_to_string (d : int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in	
	get_nth(months, (#2 d)) ^ " " ^ (Int.toString(#3 d)) ^ ", " ^ (Int.toString(#1 d))
    end;

(* 8.  *)
fun number_before_reaching_sum (sum : int, ns : int list) =
    let fun getSum(ns : int list, rsf : int, index) =
	    if (rsf < sum) andalso ((hd ns + rsf) >= sum)
	    then index
	    else  getSum(tl ns, hd ns + rsf, index + 1);
    in getSum(ns, 0, 1)
    end;

(* 9. *)
fun what_month (d : int) =
    let val ms = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	 number_before_reaching_sum(d, ms)
    end;

(* 10. *)
fun month_range (d1: int, d2 : int) =
    if d1 > d2
    then []
    else
	what_month(d1) :: month_range(d1 + 1, d2);

(* 11. *)
fun oldest (ds : (int*int*int) list) = 
    if null ds
    then NONE
    else
	let val tl_ans = oldest(tl ds)
	in
	    if isSome tl_ans andalso is_older(valOf tl_ans, hd ds)
	    then tl_ans
	    else SOME (hd ds)
	end
	    
		
	    

				 
    
						    
	
			
