# Small programming language called Hi.
**To start REPL session run ```app/Main.hs```, to end session enter ```goodbye```**
1) Numbers and arithmetic  

*Supports rational numbers, basic arithmetic and errors*  
The following sessions are possible: 
```
hi> 100  
100  

hi> -15  
-15  

hi> add(100, -15)  
85  

hi> add(3, div(14, 100))  
3.14  

hi> div(10, 3)  
3 + 1/3  

hi> sub(mul(201, 11), 0.33)  
2210.67  
```

2) Booleans and comparison  

*Boolean algebra:*  
not(true) evaluates to false (negation)  
and(true, false) evaluates to false (conjunction)  
or(true, false) evaluates to true (disjunction)  

*Equality checking:*
equals(10, 10) evaluates to true
equals(false, false) evaluates to true
equals(3, 10) evaluates to false
equals(1, true) evaluates to false (no implicit cast)

*Comparisons:*
less-than(3, 10) evaluates to true
less-than(false, true) evaluates to true
less-than(false, 0) evaluates to true (Bool is less than Number)

*Complements:*
for all A B, greater-than(A, B) ≡ less-than(B, A) holds
for all A B, not-equals(A, B) ≡ not(equals(A, B)) holds
for all A, B, not-less-than(A, B) ≡ not(less-than(A, B)) holds
for all A, B, not-greater-than(A, B) ≡ not(greater-than(A, B)) holds

*Branching:*
for all A B, if(true, A, B) ≡ A holds
for all A B, if(false, A, B) ≡ B holds

The following sessions are possible:
```
hi> false
false

hi> equals(add(2, 2), 4)
true

hi> less-than(mul(999, 99), 10000)
false

hi> if(greater-than(div(2, 5), div(3, 7)), 1, -1)
-1

hi> and(less-than(0, 1), less-than(1, 0))
false
Note also that functions are values:

hi> if(true, add, mul)
add

hi> if(true, add, mul)(10, 10)
20

hi> if(false, add, mul)(10, 10)
100
Functions can also be tested for equality:

hi> equals(add, add)
true

hi> equals(add, mul)
false
The check is trivial: a function is equal only to itself.
```
3) Operators

*Support for infix operators. The precedence and associativity are the same as in Haskell*
*For all A B:*  
A / B parses to div(A, B)
A * B parses to mul(A, B)
A + B parses to add(A, B)
A - B parses to sub(A, B)
A < B parses to less-than(A, B)
A > B parses to greater-than(A, B)
A >= B parses to not-less-than(A, B)
A <= B parses to not-greater-than(A, B)
A == B parses to equals(A, B)
A /= B parses to not-equals(A, B)
A && B parses to and(A, B)
A || B parses to or(A, B)

The following sessions are possible:
```
hi> 2 + 2
4

hi> 2 + 2 * 3
8

hi> (2 + 2) * 3
12

hi> 2 + 2 * 3 == (2 + 2) * 3
false

hi> 10 == 2*5 && 143 == 11*13
true
```
4) Strings and slices

*Basic string functions:*  
length("Hello World") evaluates to 11
to-upper("Hello World") evaluates to "HELLO WORLD"
to-lower("Hello World") evaluates to "hello world"
reverse("stressed") evaluates to "desserts"
trim(" Hello World ") evaluates to "Hello World"

"Hello" + "World" evaluates to "HelloWorld"
"Cat" * 5 evaluates to "CatCatCatCatCat" (tip: use stimes)
"/home/user" / "hi" evaluates to "/home/user/hi"

When a string is used as a function of one argument, perform a lookup:
"Hello World"(0) evaluates to "H"
"Hello World"(7) evaluates to "o"

Out-of-bounds indexing returns null:
"Hello World"(-1) evaluates to null
"Hello World"(99) evaluates to null

When a string is used as a function of two arguments, take a slice:
"Hello World"(0, 5) evaluates to "Hello"
"Hello World"(2, 4) evaluates to "ll"

When a slice index is negative, implement the Python semantics of indexing from the end of the string:
"Hello World"(0, -4) evaluates to "Hello W"
"Hello World"(-4, -1) evaluates to "orl"

When a slice index is null, treat it as the start/end of the string:
"Hello, World"(2, null) evaluates to "llo, World"
"Hello, World"(null, 5) evaluates to "Hello"

The following sessions are possible:
```
hi> to-upper("what a nice language")(7, 11)
"NICE"

hi> "Hello" == "World"
false

hi> length("Hello" + "World")
10

hi> length("hehe" * 5) / 3
6 + 2/3
```
5) Lists and folds
*Support for lists and folds*
range(5, 10.3) evaluates to [5, 6, 7, 8, 9, 10]
fold(add, [11, 22, 33]) evaluates to 66
fold(mul, [11, 22, 33]) evaluates to 7986
fold(div, [11, 22, 33]) evaluates to 1/66 (left fold)

Then overload existing operations to work on lists:
length([1, true, "Hello"]) evaluates to 3
reverse([1, true, "Hello"]) evaluates to ["Hello", true, 1]
[1, 2] + [3, 4, 5] evaluates to [1, 2, 3, 4, 5]
[0, "x"] * 3 evaluates to [0, "x", 0, "x", 0, "x"] (tip: use stimes)

When a list is used as a function, perform indexing/slicing:
["hello", true, "world"](1) evaluates to true
["hello", true, "world"](1,3) evaluates to [true, "world"]

The following sessions are possible:
```
hi> list(1, 2, 3, 4, 5)
[ 1, 2, 3, 4, 5 ]

hi> fold(add, [2, 5] * 3)
21

hi> fold(mul, range(1, 10))
3628800

hi> [0, true, false, "hello", "world"](2, 4)
[ false, "hello" ]

hi> reverse(range(0.5, 70/8))
[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]
```
6) Bytes and serialisation
*Support for bytes and serialisation*
pack-bytes([ 3, 255, 158, 32 ]) evaluates to [# 03 ff 9e 20 #]
unpack-bytes([# 10 20 30 #]) evaluates to [16, 32, 48]
encode-utf8("Hello!") evaluates to [# 48 65 6c 6c 6f 21 #]
decode-utf8([# 48 65 6c 6c 6f #]) evaluates to "Hello"
decode-utf8([# c3 28 #]) evaluates to null (invalid UTF-8 byte sequence)
zip compresses the bytes using the zlib package (specify bestCompression)
for all A, unzip(zip(A)) ≡ A holds
for all A, deserialise(serialise(A)) ≡ A holds
[# 00 ff #] + [# 01 e3 #] evaluates to [# 00 ff 01 e3 #]
[# 00 ff #] * 3 evaluates to [# 00 ff 00 ff 00 ff #] (tip: use stimes)
[# 00 ff 01 e3 #](1) evaluates to 255
[# 00 ff 01 e3 #](1,3) evaluates to [# ff 01 #]

The following sessions are possible:
```
hi> pack-bytes(range(30, 40))
[# 1e 1f 20 21 22 23 24 25 26 27 28 #]

hi> zip(encode-utf8("Hello, World!" * 1000))
[# 78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af
   fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88
   88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28 #]

hi> decode-utf8([# 68 69 #] * 5)
"hihihihihi"

hi> unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])
[# 01 02 03 #]
```
7) File I/O
*Suport for actions read, write e.t.c*
cwd! returns the current working directory
cd("mydir")! changes the current working directory to mydir
read("myfile")! returns the contents of myfile (use HiValueString if the contents are valid UTF-8 and HiValueBytes otherwise)
read("mydir")! returns the directory listing of mydir
write("myfile", "Hello")! writes "Hello" to myfile
mkdir("mydir")! creates a new directory mydir

The following sessions  are possible:
```
hi> mkdir("tmp")!
null

hi> read("tmp")!
[]

hi> mkdir("tmp/a")!
null

hi> mkdir("tmp/b")!
null

hi> read("tmp")!
[ "a", "b" ]

hi> write("tmp/hi.txt", "Hello")!
null

hi> cd("tmp")!
null

hi> read("hi.txt")!
"Hello"
Note that actions are just values and only ! forces their execution:

hi> read
read

hi> read("hi.txt")
read("hi.txt")

hi> read("hi.txt")!
"Hello"
```
8) Date and time
*Support for date and time*
parse-time("2021-12-15 00:00:00 UTC") + 1000 evaluates to parse-time("2021-12-15 00:16:40 UTC") (use addUTCTime)
parse-time("2021-12-15 00:37:51.000890793 UTC") - parse-time("2021-12-15 00:37:47.649047038 UTC") evaluates to 3.351843755 (use diffUTCTime)

The following sessions are possible:
```
hi> now!
parse-time("2021-12-15 00:42:33.02949461 UTC")

hi> parse-time("2021-01-01 00:00:00 UTC") + 365 * 24 * 60 * 60
parse-time("2022-01-01 00:00:00 UTC")
```
9) Random numbers
*Support for random*
rand(0, 5)! evaluates to 0, 1, 2, 3, 4, or 5

The following sessions are possible:
```
hi> rand
rand

hi> rand(0, 10)
rand( 0, 10 )

hi> rand(0, 10)!
8

hi> rand(0, 10)!
3
```
10) Short-circuit evaluation
*Support for short-circuit evaluation*
If(true, A, B) does not evaluate B, and if(false, A, B) does not evaluate A.

Then generalise A && B as follows:
if A is false or null, return A without evaluating B
otherwise, evaluate and return B
Generalise A || B as follows:
if A is false or null, evaluate and return B
otherwise, return A without evaluating B

The following sessions are possible:
```
hi> echo
echo

hi> echo("Hello")
echo("Hello")

hi> echo("Hello")!
Hello
null

hi> "Hello"(0) || "Z"
"H"

hi> "Hello"(99) || "Z"
"Z"

hi> if(2 == 2, echo("OK")!, echo("WTF")!)
OK
null

hi> true || echo("Don't do this")!
true

hi> false && echo("Don't do this")!
false

hi> [# 00 ff #] && echo("Just do it")!
Just do it
null
```
