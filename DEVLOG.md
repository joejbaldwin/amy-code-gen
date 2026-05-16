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
I used a loop instead of map so i can read it better, maps where confusing me.

Session 5

Implementing match

Every matchAndBind produces wasm code that when run must follow this rule:
Input (precondition): the scrutinee value is on top of the wasm stack.
Output (postcondition): the scrutinee has been consumed, and exactly one of these is on the stack:

1 if the pattern matched
0 if it didn't
 
wildcard is easy just drop the scrut and push 1 to stack

case class is harder. The scrutinee is on the stack and it's a memory address pointing to an ADT instance laid out as [index, field_0, field_1, …]

i need to check the constructor index matches and if yes, recursively check each subpattern against the corresponding field.
