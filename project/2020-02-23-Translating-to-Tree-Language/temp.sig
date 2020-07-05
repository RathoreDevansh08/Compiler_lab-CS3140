signature TEMP = sig

    (* from section 6.2, Pg.140, Appel book *)

    eqtype temp
    val newtemp : unit -> temp
    structure Table : TABLE sharing type Table.key = temp
    val makestring: temp -> string
    type label = Symbol.symbol
    val newlabel : unit -> label
    val namedlabel : string -> label
end
