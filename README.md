# The Pisp Programming Language

<img align="center" src="https://github.com/pkaterski/Pisp/blob/master/img/logo.jpg" width="400">

It's a lisp-inspired functional programming language with a python-like syntax.

A language of the future but also from the past ;)

A very simple concept - everything is a statement and every statement is either a value or a definiton!

some examples:
```python
# comments

# bools
true
false

# integers
-1
0
1
2
3

# doubles
-1.1
0.5
1.0

# strings
"hohoho"

# lists
[1 2 3 4 5]
["a" 1 5.5]

# variables / defined or not / -- everything thats not a keyword
a
hoho

# keywords if/else, cond/case, def, lambda

# if statement
# if VALUE: VALUE else: VALUE
if true:
  1
else:
  2

if true: 1 else: 2

# cond statements
cond:
  case 1 == 2: "one"
  case 2 == 2: "two"
  case 3 == 3: "three"
  else: "opa"

# definitions
def x:
  1

def y: 2

def z: add(z 1)

def f x:
  add(x x)

def g x y z:
  add(mul(x y) z)

# lambdas / can be passed around as annonimous functions
lambda x:
  sum(x 1)

foldl(lambda acc cur: cons(cur acc) [] [1 2 3])

# function calls
f(x)
g(x y z)

# special notation
x@f == f(x)


# lambda calls
(lambda x: add(x 1))(1) == 2
(lambda x y: add(x y))(1 2) == 3


# Checkout the prelude for some examples
# You can also check the examples folder / WIP /


# IO
print(1 x) == x # side effect: 1 is printed
debug(x) == x # side effect: x is printed
input == ? # a variable with a side effect: user input, each time when called it is evaluated
# checkout ./examples/IO.pisp for how to use this

```

### REPL

Pisp provides a REPL where you can play around:

<img align="center" src="https://github.com/pkaterski/Pisp/blob/master/img/repl.png" width="600">

### Interpret Files

Here is an example of the file `examples/IO.pisp` and the output when interpreted.
```python
debug("What is your name?")
def i: input
i

debug(map(strToChars [
    "it's so wonderful to meet you, "
    i
    "!"
    ]
  )@concat@strFromChars)
debug("And never forget - There shall only be FP!!1 The OOP has to die.")
debug("Long live Pisp!!")
```
output:
<img align="center" src="https://github.com/pkaterski/Pisp/blob/master/img/file_IO_example.png" width="600">


