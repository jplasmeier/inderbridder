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

Done:

* implement begin

TODO: 

* test 5 - fix global reference 
* implement if/else (scoped)
* implement break
* implement continue
* implement try/catch/finally and throw
* implement globals 
* type? function for try/catch/finally, the rest are trivial

#### State Changes

We need to convert the state from a list of positional keys and values to a list of stack frames.

This adds a layer to the stack operations, which currently are `eval-ass` and `eval-decl`. The former searches for a key and on finding it, set its value to a given value. The latter adds a key and either a value or the empty list if no value is given. 

The continuation should not be a continuation of function calls but of state operations. Since the goal of the program is to convert functions into state operations, it makes sense that our representation of the program's state is, well, just that. 

##### Global vs Local State

Currently, Test 5 is failing because it declares a variable inside a scope and the correctly returns it. This is because when a new scope is entered, we merely push a new frame onto the state and call interpret on the scoped expression and that state. In order to have a local scope, there are a few modifications that can be made. 

First, we could apply a pop-frame call to the continuation being returned from the scoped expression. Thereafter when the continuation is called it will not include that frame.  

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
# I am pretty sure that the state should begin in this state at the outer lever of program execution. A new stack frame is pushed on only when a new scope is entered. 
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

### Implementation Details

#### While Loops

While loops are a little bit tricky. The expression for a while loop is of the form:

`(while (< x 10) (begin (if (> result 15) (begin (return result)))))`

We only want to push one frame for the loop, but execute the body of the `begin`  expression repeatedly. 

#### Return

The test that is currently failing uses a return block inside a block. This doesn't work with the current approach. Normally, when a return expression shows up, the result of the continuation applied to the empty state is returned. However, if a return expression occurs inside a scoped block, more operations will be built on top of the return expression. 

As a solution, when `return` is found, set the return-cont to the current state-cont and recurse. When `null` is found, call return-cont. 

```
var x = 20;
if (x < 25) {
	var y = 15;
	return y;
}
return x;

((eq? (if-sym (car pt)) 'if) (interpret (cdr pt) (eval-if (car pt) (state-cont e-s) (lambda (v) (state-cont v)) return-cont) return-cont))
(interpret (cdr pt) (eval-if (car pt) (state-cont e-s) (lambda (v) (state-cont v)) return-cont) return-cont)
(interpret ((return x)) (eval-if (if (< x 25) (begin (var y 15) (return y))) (lambda (v) (state-cont v)) rc) rc)
; if-bool is true, interpret the then body
(interpret ((return x)) (interpret ((var y 15) (return y)) (lambda (v) (state-cont v)) return-cont)))
; now evaluate the body with interpret
(interpret ((begin (var y 15) (return y))) (lambda (v) (state-cont v)) return-cont)


```

### James' Interpreter

I'm looking at James' Interpreter project for inspiration on how to implement the requirements for Part 2. James used `call/cc` instead of CPS.