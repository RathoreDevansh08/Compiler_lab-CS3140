# Support for MIPS architecture.

**NOTE:**This weeks assignment has only the project component and
hence all code that you write as part of this assignment should go to
your project subdirectory of your repository.

Implement a module for captures the following as algebraic types.

1. The registers of MIPS

2. The MIPS assembly language instructions parameterised on labels and
   temps.

3. Functions to convert the labels, temps, and instructions
   to MIPS assembly language strings (pretty printing).

I would suggest the following form.

```
structure MIPS = struct

datatype regs = ...

datatype  ('l,'t) inst = ...

(*
   Converts an instruction where the labels are strings and temps are
   actual machine registers to a string. This function is used at the
   end to emit the actual code.

   The pretty function has the type:

   val pretty : (string, regs) inst -> string

*)


fun pretty i = ...

end

```

The suggested location for this module is in `target/mips.sml` file
under your project subdirectory. Adjust the `.mlb` and `.cm` files
appropriately.

This will be used in the subsequent assignments for things like
instruction selection.

You can assume that you are working with [SPIM] in which case
the follow <http://www.dsi.unive.it/~arcb/LAB/spim.htm> as your
quick guide to MIPS assembly.

## The `Temp` structure

To manage the generation of temporary variables and labels you should
start by coding up your `Temp` structure with the following signature

```
signature TEMP = sig
	type temp  (* temporary variables of your program *)
	type label (* temporary labels for your program *)
	val newlabel : unit -> label
	val newtemp  : unit -> temp

end
```

The advantage of defining the instructions parameterised over label
and temp type is that while generating code (for which register
allocation is not done yet), you can generate instructions of the type
`(label,temp) inst`. Register allocation can then convert this to
`(label,register) inst`.

[SPIM]: <http://www.dsi.unive.it/~arcb/LAB/spim.htm>

