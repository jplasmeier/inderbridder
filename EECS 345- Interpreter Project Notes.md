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
* implement begin
* implement break
* implement continue
* implement try/catch/finally and throw
* implement globals 
* type? function for try/catch/finally, the rest are trivial

#### State Changes

We need to convert the state from a list of positional keys and values to a list of stack frames.

This adds a layer to the stack operations, which currently are `eval-ass` and `eval-decl`. The former searches for a key and on finding it, set its value to a given value. The latter adds a key and either a value or the empty list if no value is given. 

The continuation should not be a continuation of function calls but of state operations. Since the goal of the program is to convert functions into state operations, it makes sense that our representation of the program's state is, well, just that. 

#### Current State

Currently the state contains a list of keys and a list of values:

```
state = (()())

x = 3
y = 5
z = 45

state = ((z y x) (45 5 3))
```

In order to implement the changes, the state will now be a list of lists of lists:

```
# I am pretty sure hthat the state should begin in this state at the outer lever of program execution. A new stack frame is pushed on only when a new scope is entered. 
state = ((()()))

x = 3
y = 5
z = 45
begin {
	z = 6
	w = 12
}

state = ( ((z w) (6 12)) ((z y x) (45 5 3)) )
```



### Functional Changes

Why is it that stateful behaviour necessary to implement global variables, but that CPS is sufficient for break/exceptions/local return/etc.? The reason is that one could implement globals without using state, I think, but it would be through an outrageously complicated continuation built up for the state. Each operation to the state would modify a continuation which, when evaluated, produces the state associated with the data of that function. However, in this case, it is much easier to use set-box!. 

### James' Interpreter

I'm looking at James' Interpreter project for inspiration on how to implement the requirements for Part 2. James used `call/cc` instead of CPS.