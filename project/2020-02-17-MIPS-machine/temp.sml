signature TEMP = sig
    type temp  (* temporary variables of your program *)
    type label (* temporary labels for your program *)
    val newlabel : unit -> label
    val newtemp  : unit -> temp

end

val labellist = ["abc"]
val templist  = ["bcd"]

structure Temp : TEMP = struct
	type temp  = string
	type label = string
	
	(* Diagnolization to create new labels *)
	fun change "a" = "b" 
  	  | change _   = "a"
  	  
  	fun diag [] _      = "a"
      | diag (x::xs) i = if ((size x)>i) then (change (substring (x, i, 1))) ^ (diag (xs) (i+1))
                         else diag xs i
                         
    fun fresh ls = diag ls 0
	
	(* Creating new labels, targets based on previous targets *)
	fun newlabel () = fresh labellist 
	fun newtemp  () = fresh templist

end

val id = Temp.newlabel () ;
