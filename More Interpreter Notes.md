## 1

### 1-1
```
return 150;

((return 150))
```
((eq? (car (car pt)) 'return)

## 2

#### 2-1

```
((var x 10) (begin (var y 2) (var z (* x y)) (= x z)) (return x))

(lambda (v) (eval-return 'x (begin-frame '((var y 2)(var z (* x y))(= x z)) (eval-decl (var x 10) v)

(State-add (x 10)) -> ((((x)(10)))

(eval-decl (var y 2) (Push-Frame (eval-decl (var x 10) v)))

(eval-decl (var z (* x y)) (eval-decl (var y 2) (Push-Frame (eval-decl (var x 10) v)))

```

#### 2-5

Should give an error, not a return value. It's because `min` is being declared inside the scope of the if/else block but being dereferenced from outside that scope. Currently, all variables are implicitly declared as global variables. As it stands now, I think there are two ways to fix this. 

The first way is to pop the frame off of the state-cont when the execution of a scoped expression terminates. 

The second way is to modify derefence to somehow know that `min` is not in the current scope. This might now actually be possible...

```
var x = 10;
var y = 4;
if (x < y) {
  var min = x;
}
else {
  var min = y;
}
return min;

(eval-if '(if (< x y) (begin (var min x)) (begin (var min y))) (lambda (v) (eval-decl '(var y 4) (eval-decl '(var x 10) v))))

# eval-if
expr: (if (< x y) (begin (var min x)) (begin (var min y)))
state-cont: (lambda (v) (eval-decl '(var y 4) (eval-decl '(var x 10) v))))

((interpret (if-elsex expr) state-cont) e-s)
(interpret ((begin (var min y))) state-cont)
(interpret '() (interpret '((var min y)) (lambda (x) (push-frame (state-cont x))))

# interpret
pt: ((var min y))
state-cont: (lambda (v) (push-frame (eval-decl '(var y 4) (eval-decl '(var x 10) v))))
```

#### 2-6

```
((var x 0) (var result 0) (while (< x 10) (begin (if (> result 15) (begin (return result))) (= result (+ result x)) (= x (+ x 1)))) (return result))

# state-cont
(lambda (v) (eval-decl '(var x 0) v))
# state
(((x)(0)))

# state-cont
(lambda (v) (eval-decl '(var result 0) (eval-decl '(var x 0) v)))
# state
(((result x)(0 0)))

# state-cont
(lambda (v) (eval-decl '(var result 0) (eval-decl '(var x 0) v)))
# state
(((result x)(0 0)))

```