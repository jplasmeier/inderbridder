# EECS 345: Interpreter Project Notes

## General 

Order:

* interpret
* null
* return
* decl
* ass
* if
* while
* name
* value
* bool
* error

* main
* helpers
* type?
* eval

## Part 1

TODO:

* refactor isUnary? and similar code in if
* fix tests :)
* consider refactor listOfOne?
* consider refactor eval-bool-or-value 
* should be value -> bool or number
* use 'interpret' function more?
* pull out state operations

## Part 2

TODO: 

* state to list of state layers
* make interpret tail recursive 
* type? functions for begin, try/catch/finally, break, continue, throw

#### State Changes

We need to convert the state from a list of positional keys and values to a list of stack frames.

This adds a layer to the stack operations, which currently are `eval-ass` and `eval-decl`. The former searches for a key and on finding it, set its value to a given value. The latter adds a key and either a value or the empty list if no value is given. 

Why is it that stateful behaviour necessary to implement global variables, but that CPS is sufficient for break/exceptions/local return/etc.? The reason is that one could implement globals without using state, Ithink, but it would be through an outrageously complicated continuation built up for the state. Each operation to the state would modify a continuation which, when evaluated, produces the state associated with the data of that function. 