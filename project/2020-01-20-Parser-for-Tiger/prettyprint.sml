structure PrettyPrint =
struct

val indent = ref 0
val bktab = "\b\b\b\b";

(* colours for different type of tokens *)
val rest = "\027[0m";
val blue = "\027[0;34m";        (* strings *)
val cyan = "\027[0;36m";        (* numericals *)
val green = "\027[0;32m";       (* identifiers *)
val red = "\027[0;31m";         (* operators *)
val yellow = "\027[0;33m";      (* keywords *)

fun get_tabs n = if n = 0 then "" else "    " ^ get_tabs (n-1)
fun new_line n = ("\n" ^ get_tabs(n))

fun print_expr (Ast.INT x) = print (Int.toString (x))
|   print_expr (Ast.ID x) = print (x)
|   print_expr (Ast.BINOP (x , bop , y)) = ( print_expr x ;
        								     print (" " ^ (Ast.binOpToString bop) ^ " ") ;
        								     print_expr y )
| 	print_expr (Ast.FUNC(a, b)) = ( print (a ^ " (");
							  	    print_expcomm (b);
								    print (")") )
| 	print_expr (Ast.LET (x, y)) = ( indent := !indent + 1;	
								    print ("let" ^ new_line(!indent));
								    print_decs (x);
								    print ("in" ^ new_line (!indent));	
								    print_exps (y);
								    indent := !indent - 1;
								    print ("end") )
| 	print_expr (Ast.IF(a, b)) = ( print ("if (");
								  print_expr (a);
								  print (") then (");
								  indent := !indent + 1;
								  print (new_line(!indent));
								  print_expr (b);
								  indent := !indent - 1;
								  print (new_line(!indent) ^ ")") )
| 	print_expr (Ast.IFELSE(a, b, c)) = ( print ("if (");
 									     print_expr (a);
										 print (") then (");
										 indent := !indent + 1;
										 print (new_line(!indent));
										 print_expr (b);
										 indent := !indent - 1;
										 print (new_line(!indent));
										 print (") else (");
										 indent := !indent + 1;
										 print (new_line(!indent));
										 print_expr (c);
										 indent := !indent - 1;
										 print (new_line(!indent));
										 print (")") )
|	print_expr (Ast.FOR(i, a, b, c)) = ( print("for");
							 indent := !indent + 1;
							 print (new_line(!indent));
							 print (i ^ " := ");
							 print_expr (a);
							 print (" to ");
							 print_expr (b);
							 indent := !indent - 1;
							 print (new_line(!indent));
							 print("do");
							 indent := !indent + 1;
							 print (new_line(!indent));
							 print_expr (c);
							 indent := !indent - 1;
							 print (new_line(!indent))
							 )						 
|	print_expr (Ast.WHILE(a,b)) = ( print("while");
						  indent := !indent + 1;
						  print (new_line(!indent));
						  print_expr (a);
						  indent := !indent - 1;
						  print (new_line(!indent));
						  print("do");
						  indent := !indent + 1;
						  print (new_line(!indent));
						  print_expr (b);
						  indent := !indent - 1;
						  print (new_line(!indent))
						  )
|	print_expr (Ast.BREAK) = (print("break"))						  

and

	print_expcomm (x :: xs) = (	print_expr (x);
								if (null xs) then () else print (", ");
								print_expcomm (xs) )
|   print_expcomm ([]) = ()

and

	print_exps (x::exp_lst) = ( print_expr (x);
								if (null exp_lst) then () else print (";");
								print (new_line(!indent));
								print_exps(exp_lst) )
|   print_exps [] = ( print (bktab) )

and 

	print_decs (x :: y) = (	print_dec(x);
							if (null y) then print ("") else print (";");
							print (new_line(!indent));
							print_decs(y) )
|   print_decs [] =	( print (bktab) )

and 

	print_dec (Ast.VARDECL(x, y)) = ( print ("var " ^ x ^ " := ");
		 							  print_expr(y) )

fun compile []        = ()
  | compile (x :: xs) = (print_expr x ; print (";" ^ new_line (!indent)) ; compile xs)

end
