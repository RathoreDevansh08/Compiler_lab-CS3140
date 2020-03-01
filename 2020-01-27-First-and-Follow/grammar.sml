type Symbol = Atom.atom
type Token  = Atom.atom

type Symbols = AtomSet.set   (* set of symbols *)
type Tokens  = AtomSet.set   (* set of tokens  *)

type RHS    = Atom.atom list  (* The RHS γ of a rule A -> γ *)

(*

We have the structures AtomSet and AtomMap to represent sets and maps
of Atoms. For any type t if we want sets and maps (dictionaries) we
need an ordering structure on the elements.  We would like to create
the set structure on RHS's. For this you first need to define a
structure of signature ORD_KEY for RHS.

*)

structure RHS_KEY : ORD_KEY = struct
    type ord_key = Atom.atom list
    fun compare (a , b)  = case (a , b) of
                            ([], [])    => EQUAL
                        |   ([], x::xs) => LESS
                        |   (x::xs, []) => GREATER
                        |   (x::xs, y::ys)
                                        =>  let
                                                val temp = Atom.lexCompare(x,y)
                                            in
                                                case temp of
                                                    EQUAL   => compare(xs , ys)
                                                |   GREATER => GREATER
                                                |   LESS    => LESS
                                            end
end


(* Use the above structure to create a set of rhs's *)

structure RHSSet = RedBlackSetFn (RHS_KEY)

type Productions = RHSSet.set

(* The rules of the grammar are a dictionary whose keys are the symbol
   and the values are the Productions associated with the grammar.
*)

type Rules = Productions AtomMap.map


type Grammar    = { symbols : Symbols, tokens : Tokens, rules : Rules }


(*
			S -> E$
            E -> E + T | T
            T -> T * F | F
            F -> id | num | ( E )  
*)


(* Set of atoms to represent the symbols in the grammar. *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList (!sym, [Atom.atom "S", Atom.atom "E", Atom.atom "T", Atom.atom "F"]);

(* Set of atoms to represent the tokens in the grammar. *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList (!tok, [Atom.atom "num", Atom.atom "id", Atom.atom "(", Atom.atom ")", Atom.atom "*", Atom.atom "+", Atom.atom "$"]);

(* Creating set for each symbol rules*)
val S_ = ref RHSSet.empty;
S_ := RHSSet.add (!S_, [Atom.atom "E", Atom.atom "$"]);

val E_ = ref RHSSet.empty;
E_ := RHSSet.add (!E_, [Atom.atom "E", Atom.atom "+", Atom.atom "T"]);
E_ := RHSSet.add (!E_, [Atom.atom "T"]);

val T_ = ref RHSSet.empty;
T_ := RHSSet.add (!T_, [Atom.atom "T", Atom.atom "*", Atom.atom "F"]);
T_ := RHSSet.add (!T_, [Atom.atom "F"]);

val F_ = ref RHSSet.empty;
F_ := RHSSet.add (!F_, [Atom.atom "id"]);
F_ := RHSSet.add (!F_, [Atom.atom "num"]);  
F_ := RHSSet.add (!F_, [Atom.atom "(", Atom.atom "E", Atom.atom ")"]);

(* rules *)
val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
rule := AtomMap.insert (!rule, Atom.atom "T", !T_);
rule := AtomMap.insert (!rule, Atom.atom "F", !F_);


val Grm : Grammar = { symbols = !sym, tokens = !tok, rules = !rule };

