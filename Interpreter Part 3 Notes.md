# EECS 345: Interpreter Part 3 Notes

Function Calls.

### Refactoring to do from part 2

##### Abstract out push/pop of stack frames.

- this might not be possible 

##### Get 17 to pass.

More specifically, fix this: 

`(eval-ass-s '(= x (+ i x)) '((() ()) (() ()) ((i) (10)) ((j x) (1 0))))`

Done

##### State vs State-cont

Defer still takes state :(

### Step Through Examples

Emphasis on how state is represented and modified within function calls.

### General Ideas

Will need a separate state/part of the state for globals

Execute "outer" layer then call main (only assignment is permitted), return main

Nested function calls.

The implementation of this is about environments. When a function is called, it gets the variables in scope, globals, its parameters, and then executes the body.

Note that the functionParser parser will cause tests from pt1, pt2 to fail.

### Actual Design

Treat functions like variables. That is, when a function is defined in the base scope, it is a global variable. 

`funcall` makes a call to deref in order to find the scope contents of that function name. 

A function call should interpret the body of that function. The state passed in should be the globals, the current stack frame, and the parameters. Function calls should return the entire state with the additional modifications affected by the function. 

Within the actual function call, should the function get:

* the whole state, and in its call only touch parts it has access to
* just the relevant parts, and apply changes to globals to the "whole" state 

I think just the relevant parts make the most sense- I'd rather not have to rewrite eval-ass, etc. 

### State Changes

Now we have actual globals, function definitions, function environments, etc. We can probably get away with squashing everything into the existing `state-cont` or we can have separate objects for globals, stack frames, etc. and pass around more parameters. 

I think putting function defintions in globals makes sense. 

### Misc Notes

Should make helper functions for parameter binding and function body getting.

#### TODO

Implement function definiton. 

Implement new execution order (base, main)

#### Vocabulary

* Formal Parameter: the identifier used in a method to stand for the value that is passed into the method by a caller.
* Actual Parameter: the actual value that is passed into the method by a caller.
* Argument: the data passed into the function call.

