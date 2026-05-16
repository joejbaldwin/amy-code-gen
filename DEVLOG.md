# DEVLOG lab 5

During finals so this devlog will be more rushed.

Session 1: Yesterday read and understood (mostly) what is needed to be done. 

Session 2: Making reference to instructions.scala and utils.scala.
I implemented the simple cases covering the usual AST nodes like add, sub etc (note that amydiv is added when there is a naming conflict with webassebly)

Session 3:

Adding the variable case: we need the value of "x" (name) to be on the stack and, we know the variable x is on the locals, so i need to look it up in locals map to find the slot number its stored at. 

added implement Variable, Ite, Sequence, Let"

Session 4:

Implemented the Call case, which handles both function calls and constructor calls (e.g. Cons(1, Nil())).

For a normal function call i push all the arguments onto the stack left-to-right, then emit a wasm call instruction using the function's name

For a constructor call: we need to build the ADT in heap memory manually. Save the current memory boundary as the base address, bump the boundary forward by (1 + number of fields) * 4 bytes to reserve space, store the constructor's index at the base, then store each field value at base + 4*(i+1). Finally push the base address as the "value" of this ADT. We distinguish functions from constructors by checking the symbol table.