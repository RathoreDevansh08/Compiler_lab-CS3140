use "grammar.sml";

val flag = ref true;

(* pretty printing colours *)
val rest = "\027[0m";
val blue = "\027[0;34m";    
val cyan = "\027[4;36m";    

(* empty FIRST, FOLLOW, NULLABLE : 'a map *)
val FIRST : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val FOLLOW : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val NULLABLE : bool AtomMap.map ref = ref AtomMap.empty;

(* function to check if 's' is in NULLABLE *)
fun is_null (s) = AtomMap.lookup (!NULLABLE, s) handle NotFound => false

(* function to check for nullable *)
fun is_nullable (x :: xs) = is_null (x) andalso is_nullable (xs)
|   is_nullable ([])      = true

fun initialize x = let 
             	val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        	 in
             	NULLABLE := AtomMap.insert (!NULLABLE, x, false);
             	FOLLOW := AtomMap.insert (!FOLLOW, x, AtomSet.empty);
             	FIRST := AtomMap.insert (!FIRST, x, AtomSet.empty)
        	 end

(* function to print atomList *)
fun printAtomList (x :: xs) = 
    (
        print (Atom.toString (x) ^ " ");
        printAtomList (xs)
    )
|   printAtomList ([]) =  print " "

(* function to print production rules of grammar *)
fun print_productions x = 
    (
        let 
            val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        in
            print (Atom.toString (x) ^ ": "); 
            while (List.null(!prods) = false) do (
                let
                    val rhs = ref (List.hd(!prods))
                in
                    if (null (!rhs)) then (
                        print ("ε ")
                    ) else (
                        printAtomList (!rhs)
                    )
                end;
                prods := tl (!prods);
                if (null (!prods)) then (
                        
                ) else (
                    print ("| ")
                )
            );
            print ("\n")
        end
    );

(* Calculating nullable symbols *)
fun calculate_nullable x rhs = 
    (
        if (is_nullable(!rhs) andalso not (is_null(x))) then (
                flag := true;
                NULLABLE := #1 (AtomMap.remove (!NULLABLE, x));
                NULLABLE := AtomMap.insert (!NULLABLE, x, true)
            ) else ()
    );

(* Calculating first *)
fun calculate_first x rhs = 
    (
        let 
            val i = ref 0
            val still_nullable = ref true
            val k = List.length(!rhs)
            val c = ((AtomMap.remove (!FIRST , x)) handle LibBase.NotFound => (!FIRST, AtomSet.empty))
            val (mp , el) = (ref (#1 c) , ref (#2 c))
            val old_el = !el
        in 
            FIRST := !mp;
            while (!i < k andalso !still_nullable) do (
                let 
                    val yi = List.nth(!rhs, !i)
                in
                    el :=  AtomSet.union(!el , AtomMap.lookup(!FIRST, yi) handle NotFound => (
                        if (AtomSet.member(!sym, yi)) then (
                            AtomSet.empty
                        ) else (
                            AtomSet.singleton (yi)
                        )
                    ));
                    still_nullable := is_null(yi)
                end;
                i := !i + 1
            );
            if (AtomSet.equal (!el, old_el)) then () 
            else (
                flag := true
             );
            FIRST := AtomMap.insert (!FIRST, x, !el)
        end
    );

(* E1 -> E2 : add follow of E1 in follow of E2 *)
fun follow_rule_1 yi x = 
    (
        let 
            val c = ((AtomMap.remove (!FOLLOW , yi)) handle LibBase.NotFound => (!FOLLOW, AtomSet.empty))
            val (mp , el) = (ref (#1 c) , ref (#2 c))
            val old_el = !el
        in 
            FOLLOW := !mp;
            el := AtomSet.union (!el, AtomMap.lookup(!FOLLOW, x) handle NotFound => (AtomSet.empty));
            if (AtomSet.equal (!el, old_el)) then () 
            else (
                flag := true
            );
            FOLLOW := AtomMap.insert (!FOLLOW, yi, !el)
        end
    );

(* E1 -> ET : add first of T in follow of E *)
fun follow_rule_2 yi x = 
    (
        let 
            val c = ((AtomMap.remove (!FOLLOW , yi)) handle LibBase.NotFound => (!FOLLOW, AtomSet.empty))
            val (mp , el) = (ref (#1 c) , ref (#2 c))
            val old_el = !el
        in 
            FOLLOW := !mp;
            el := AtomSet.union (!el, AtomMap.lookup(!FIRST, x) handle NotFound => (AtomSet.singleton (x)));
            if (AtomSet.equal (!el, old_el)) then () 
            else (
                flag := true
            );
            FOLLOW := AtomMap.insert (!FOLLOW, yi, !el)
        end
    );

(* Calculating follow *)
fun calculate_follow x rhs = 
    (
        let 
            val i = ref 0
            val k = List.length(!rhs)
        in 
            while (!i < k) do (
                let 
                    val yi = List.nth(!rhs, !i)
                    val j = ref (!i + 1)
                    val still_nullable = ref true
                in
                    if (AtomSet.member(!sym, yi)) then (
                        if (!i = k - 1 orelse is_nullable (List.drop (!rhs, !i + 1))) then (
                            follow_rule_1 yi x
                        ) else ();
                        while (!j < k andalso !still_nullable) do (
                            let 
                                val yj = List.nth (!rhs, !j)
                            in 
                                follow_rule_2 yi yj;
                                still_nullable := is_null(yj)
                            end;
                            j := !j + 1
                        )
                    ) else ()
                end;
                i := !i + 1
            )
        end
    )

fun traverse_productions function x = 
    (
        let 
            val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        in
            while (List.null(!prods) = false) do (
                let
                    val rhs = ref (List.hd(!prods))
                in
                    function x rhs
                end;
                prods := tl (!prods)
            )
        end
    );

fun traverse_sym function = 
    (
        let 
            val sym = ref (AtomMap.listKeys (#rules Grm))
        in 
            while (List.null (!sym) = false) do (
                let 
                    val x = hd(!sym)
                in 
                    function x;
                    sym := tl(!sym)
                end
            )
        end
    );

(* function to print elements in atomSet *)
fun printAtomSet at_set = 
    (
        printAtomList(AtomSet.listItems(at_set))
    );

(* function to print all the input productions *)
fun print_all_productions () = 
    (
        print (cyan ^ "\n=== PRODUCTIONS ===\n" ^ blue);
        traverse_sym print_productions;
        print (cyan ^ "= = = = = = = = = = \n" ^ rest)
    );

(* function to facilitate first/follow lists *)
fun print_list [] =  
    (
        print (cyan ^ "= = = = = = = \n" ^ rest)
    )
|   print_list (x::xs) = 
    (
        let
            val (k , v) = x
        in
            print ((Atom.toString k) ^ " : " );
            printAtomSet(v);
            print ("\n");
            print_list xs
        end
    );

fun print_term [] = print ""
|   print_term (x::xs) = 
		(
			print ((Atom.toString x) ^ " : " ^ (Atom.toString x));
			print("\n");
			print_term xs
		);

(* function to print the follow elements *)
fun printFollow () = 
        let
            val follow_lst = AtomMap.listItemsi (!FOLLOW)
        in
            print (cyan ^ "\n=== FOLLOW ===\n" ^ blue);
            print_list follow_lst
        end

(* function to print the first elements *)
fun printFirst () = 
      ( let
            val first_lst = AtomMap.listItemsi (!FIRST)
        in
            print (cyan ^ "\n=== FIRST ===\n" ^ blue);
            print_list first_lst
        end );

fun evaluate function = 
    (
        flag := true;
        while (!flag = true) do (
            flag := false;
            traverse_sym (traverse_productions (function))
        )
    );

(* --- main --- *)
print_all_productions();
traverse_sym initialize;

evaluate calculate_nullable;
evaluate calculate_first;
evaluate calculate_follow;

printFirst();
printFollow()

