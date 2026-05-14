# DEVLOG lab 5

During finals so this devlog will be more rushed.

Session 1: Yesterday read and understood (mostly) what is needed to be done. 

Session 2: Making reference to instructions.scala and utils.scala.
I implemented the simple cases covering the usual AST nodes like add, sub etc (note that amydiv is added when there is a naming conflict with webassebly)

Session 3:

Adding the variable case: we need the value of "x" (name) to be on the stack and, we know the variable x is on the locals, so i need to look it up in locals map to find the slot number its stored at. 

added implement Variable, Ite, Sequence, Let