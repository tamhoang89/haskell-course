# Data types, Signatures, and Polymorphism


## Outline

- Pragmatic intro to types
- Function’s signatures
- Playing with functions
    - Variables in Haskell
    - Infix and prefix functions
- Common Data Types
- Polymorphic values and type variables
- Fun with lists!

## Pragmatic intro to types

### The `::`

A type is a label that every expression has and restricts its usage.

We use a *double colon* `::` to show or assign the type of expression. For instance, the code

``` haskell
myexpression :: MyType 
```

tells us that the expression `myexpression` has type of `MyType`.  

### Frequently used types

There are the standard types that are frequently used in Haskell:

* `Int` and `Integer` for integer numbers.
* `Float` and `Double` for floating point real numbers.
* `Bool` for `True` and `False`.
* `Char` for characters.
* `String` for strings of text.

### How to check the type?


The `:type` command (or `:t` for short) in GHCI, followed by any valid expression, tells us its type.



```haskell
:type True

:type False

:t (3 < 5)

:t 'A'

:t "Hello world!"
```

## Signature of a function

The double colon symbol `::` should be read as simply "is of type", and indicates a type *signature*. Let's explain what is a type signature by the following example. In Haskell, a function that squares its argument is defined as:


```haskell
square :: Int -> Int
square v = v * v
```

The first line contains the **signature**, the second line contains the **definition** of a function `square`.



* A **signature** of a function is a message to the whole world that such function exists, here is its name and here are the types it works with. 

* A **definition** of a function is information about what exactly this function does.

In the signature

```haskell
square :: Int -> Int
```

two parts are *separated* by a double colon into 


* the **name** of the function on the left and

* the **type of function** on the right. 

**All data in a Haskell program is of a particular type.** And since the function works with data, its **signature contains the types of its inputs and output, separated by arrows `->`**.

The signature of the squaring function `square` tells us that it accepts a *unique* argument of type `Int` and returns a value of the same `Int` type. 


If there are more than one argument, the signature is simply pulled out. For example, `prod` function signature, which returns the product of two integer arguments, could look like this:


```haskell
prod :: Int -> Int -> Int
prod x y = x * y
```

It has two arguments of `Int` type, and its output also has `Int` type. 



In the **definition** of a function, the equality sign `=` separates the code in two parts.



* **Head** is a code to the left of `=` and consists of the **name of a function** and **argument names** (names, not types!), separated by spaces.

* **Body** is a code to the right of `=` that expresses the essence of the function, its contents. 



### What a function's signature tells us? 



Haskell is a *functional* programming language and every program consists of *functions*. Each function takes a fixed number of parameters of some types and returns a value which also has a type. For instance, a function

``` haskell
not :: Bool -> Bool
``` 

takes a parameter of type `Bool` and returns its negation, which is again of type `Bool`. 



Looking at the *rightmost arrow `->`* in the signature, we understand that 

* everything to the left of it are **types of arguments**, that can also be separated by arrows `->`,

* everything to the right is a **type of the calculated value**.

## Playing with functions

### Variables in Haskell (Names/definitions)

Take a look at this function:


```haskell
name = "Bob"
```

If we don't have parameters, we have a function that always returns the same value—a `String`—, no matter what!

So, we have an expression of type:


```haskell
name :: String
name = "Bob"
```

**The kind of function that doesn't take parameters is usually called a definition or a name.** 

Although, you could call it variable too, since this is what in most programming languages is called a variable. But, "variable" doesn't always mean the same.

Because we can't change the value of a definition (the expression on the right side of the `=` always evaluates to the same result), `name` and `"Bob"` are essentially the same thing. And we can use them interchangeably.

When talking about programming in general, a variable is like a box that contains a value. And the variable's name is written on the side of the box. You can put values inside the box, and—in most programming languages—you can later change your mind and replace the value inside the box.

```haskell
-- THIS IS NOT VALID HASKELL!!!
x = 3
7 + x   -- 10
x = 5
7 + x   -- 12
```

OOOOOOOOhhhhhh but not with Haskell, no, no, no! Once you tell Haskell that `x` means `3`, it will mean `3` forever!

In technical terms:

Haskell's variables are **immutable.**

Haskell's concept of variable is different. Haskell has variables, but in the mathematical sense. In the sense that when we say:

```Haskell
x = 3
city = "Paris"
letter = 'a'
it'sTrue = True
```

We're stating that the term on the left of the `=` sign is **interchangeable** with the term to the right side of the `=`. 

And this also applies to the parameters of a function:


```haskell
volumeOfACylinder r h = pi * r^2 * h 
```

In this case, once we pass values to the parameters of `volumeOfACylinder`, we can't change them inside the function's body. We can use the function again and pass different parameters, but we can't *change* them once we pass them.

## Infix and prefix notations



You can apply (use) functions in two different notations: infix and prefix notations.



### Prefix



Let's look at an expression:


```haskell
prod x y = x * y
prod 4  5
```

`prod` is used in **prefix form**, that is, **before its arguments**. 

### Infix


Let's look at an expression:


```haskell
1 + 2
```

`+` is actually a function! And is written in **infix form**, that is, **between its arguments**.

Functions intended for the infix form of application are called **operators**.



And how do you know if a function is infix or prefix? Well...

Functions defined with **only symbols** will be automatically set as **infix functions**, else they're prefix functions.

But you can still use an infix function as a prefix function and vice versa.

### Infix to prefix and vice versa



We use parentheses around an infix function to use it as a prefix function:


```haskell
(+) 1 2
```

To check the type of an infix function, we also have to surround the name between parenthesis:


```haskell
:t (+)
```

<div class="alert alert-block alert-info">
I'm sure you noticed that the `+` type signature looks different from the previous ones. That's because it uses polymorphic types and type classes. We'll learn about polymorphic types today and about type classes in future lessons. For now, don't worry about it too much.
</div>

We use backticks \` around a prefix function to use it as an infix function:


```haskell
4 `prod` 5
```

## Common Data types


### Integer number types: `Int` and `Integer`


- `Integer` is an arbitrary precision type: It will hold any integer—no matter how big—up to the limit of your machine's memory.

This means you'll never have arithmetic overflows, but, it also means your arithmetic is relatively slow. 

- `Int` values, on the other hand, are bound to be in the range of $±2^{63}$ *(for 64-bit CPUs)*.

This limits the values that `Int` can hold, but it makes it more efficient.

Let's see this in practice:


```haskell
2^62 :: Int -- All good
```


```haskell
2^64 :: Int -- Oh no!
```


```haskell
2^127 :: Integer -- All good again
```

But what about real numbers? Numbers with decimal places? That's why we have `Float` and `Double`.

### Floating-point number types: `Float` and` Double`

`Float` is a real floating point with single precision (32 bits), while `Double` is a real floating point with double the precision (64 bits).

Let's see what happens if we want to show the first 20 digits of pi (π) in both types:


```haskell
3.14159265358979323846 :: Float

3.14159265358979323846 :: Double
```

You can tell that `Double` is waaaaaay more precise than `Float`.

Theoretically, the reasons as to when to use one or the other are somewhat analogous to the `Int`, and `Integer` cases. `Double` has double the precision, but hogs more memory because it uses twice as many bits to represent numbers.

BUT!

Recommendation based on real-world use: 

- **Even if you don't specially care about exact values, use `Double`.** There's rarely a speed disadvantage in modern computers, and with `Double`, you are much less likely to shoot yourself in the foot with rounding errors.

- If you're in a setting where **exact amounts are critical** (e.g., finance and accounting), a good idea would be to **use the `Rational` or `Decimal` data types**. Because they avoid rounding errors altogether. We'll cover them in future lessons.

### Boolean type `Bool`



The boolean type `Bool` only contains two values: `True` and `False`.


Numbers, characters and strings can be compared using the usual **comparison operators** to produce a `Bool` value: $$==,~~/=,~~<=,~~>=,~~<,~~>$$


```haskell

5 /= 0 -- True

3 >= 0 -- True

7.2 < 6.1 -- False

pi > 3.14 -- True
```

There are also the `&&` (**AND**) and `||` (**OR**) operators that allow us to combine values:

- The `&&` (AND) operator returns `True` if both the boolean to its left and right are `True`.
- The `||` (OR) operator returns `True` if either one of them is `True`.


```haskell
:t (&&)
:t (||)

True && False
True || False
```

### Character type `Char`

`Char` is the type we use to represent a *Unicode* character.

<div class="alert alert-block alert-info">
<p>
The Unicode standard (Unicode) is a set of rules that enforce a way of handling and expressing text. It's needed because computers think in numbers (ones and zeroes), and we have to collectively decide which numbers represent which characters.
</p>
<p>
It's actually a little more complicated (see: <a href="https://en.wikipedia.org/wiki/Character_encoding">Character encoding</a>). But for our purpose, we just want to know that we can use almost any symbol that we'll ever need by using Unicode characters. Letters, numbers, and more than 140k+ symbols.
</p>
</div>


We write values of type Char (Unicode characters) between single quotes. Like this:


```haskell
'a'
'@'
'7'
```

Note that if you write a number surrounded by single quotes (like in the last expression), Haskell won't treat it like a number. It will treat it like any other character. So, you can't do math with `'7'` (with quotes), but you can with `7` (without quotes).

<div class="alert alert-block alert-warning">
Important: You can only write a single character at a time! Something like <code>'hi'</code> isn't a valid <code>Char</code>!
</div>

So, how can you write full sentences? I'll tell you. But before that, we have to learn about lists.

### Lists

In Haskell, **lists are a homogenous data structure**.

This is just a fancy way of saying that they are lists that store elements of the same type. So, we can have a list of `Int` or a list of `Char`, but we can't have a mixed list.

* Lists are denoted by square brackets `[1,5,3,-4,0]` and the values in the lists are **separated by commas**.

* The type of list is expressed as the types of the elements that it contains, surrounded by squared brackets. A list of type `[Int]` contains numbers of type `Int`. A list of type `[Char]` contains elements of type `Char`.


```haskell
:t ['a', 'b', 'c', 'd']

:t [True,False, 3 > 2, 'a' == 'b']
```

### Strings

**Strings represent lists of characters.** You can use the `String` type to write messages, alphanumeric values, symbols, etc. Unlike `Char`s, `String`s are surrounded with **double-quotes** like this:


```haskell
"Hellooooooo!"
```

Which means that these two values are the same!:


```haskell
['H','i','!'] == "Hi!"
```

And also that `String` and `[Char]` are the same type! More specifically, `String` is syntactic sugar (syntax that is designed to make things easier to read or to express) for `[Char]`! So, you can use them interchangeably!

What you can't use interchangeably in Haskell are single and double-quotes. `String` (written in double-quotes) are lists of `Char` (written using single quotes). These are not the same!:


```haskell
:t "A"
:t 'A'
```

Every coder knows that lists are extremely useful. But what if you want to put together values of different types? That's when tuples are helpful!

### Tuples

Tuples are structures used to store **heterogeneous elements** as a single value.

We represent tuples by starting with an open parenthesis, writing all the elements separated by a comma, and ending with a close parenthesis. This is an example of a tuple with 3 elements:


```haskell
('a', 3, True)
```

It sounds a lot like lists, but there are two key differences:

* **Tuples can store elements of different types:** As you can see in the previous example, tuples can store elements of different types, while lists can't.

* **Tuples have a fixed size:** You can increase the size of lists by concatenation or other means, but you can't increase or reduce the size of a tuple. Once you indicate that a tuple has N elements, it will always have N elements.

**And those key differences are reflected in the tuple's type.**

The type of tuple depends on:

- The types of its elements.
- The order of the elements.
- The quantity of the elements.

For example:


```haskell
:t ('a', True)

:t (True, 'a')

:t (True, 'a', 'b')

:t (True)
```

As you can see, `('a', True) :: (Char, Bool)`, `(True, 'a') :: (Bool, Char)`, and `('a', True, True) :: (Char, Bool, Bool)` all have different types. As far as the compiler knows, those three tuples are as different between them as `Float` and `Char`.

Did you notice that if you try to create a single-element tuple, GHCi returns just the element? (Last two expressions of previous code block.) That's because there's no single-element tuple! Having a single-element tuple would provide no extra value. So, Haskell ignores the tuple and evaluates only the element.

## Polymorphic values and type variables



The great thing about types is that they protect us from ourselves! If we say that a function takes an input of type `[Char]`, Haskell will check that we meet that requirement each time we use that function. If we pass a `Double`, the compiler will yell at us to correct that mistake!

But now we have a problem! Imagine that we create the `prod` function:


```haskell
prod :: Int -> Int -> Int
prod x y = x * y
```

It works perfectly for values of type `Int`. But what if we need it for values of type `Dobule`? We know that it'll work because they're still numbers, and the formula will provide the correct answer.

We *could* create a new function that does the same but specifies a different type:


```haskell
prodForDubles :: Double -> Double -> Double
prodForDoubles x y = x * y
```

Technically, it works. But now, what about `Float` and `Integer` types? If we need to create duplicate functions for every case, this quickly becomes unsustainable!

**Polymorphic types to the rescue!**

Polymorphic means something that has multiple forms. And **a polymorphic value is a value that can have multiple types**. (For example, `4` can be `Int`, `Integer`, `Float`, ...)

For example, imagine that we want to create a function that takes a tuple with two values (also called pair) and returns the first value. Like this:


```haskell
first (x,y) = x
```

Which type should it have? I don't particularly care about the types of the elements because I don't do anything with them! I don't do arithmetic, text-related stuff, or anything! Furthermore, I just got the first element back, and that's it!

In these cases, we specify a signature with type variables!


```haskell
first :: (a, b) -> a
first (x,y) = x

first ('a', "hi!")
```

That signature reads: "The `first` function takes a pair of type `(a, b)` and returns a value of type `a`."

<div class="alert alert-block alert-warning">
<b>Important:</b> Specific types (i.e., <code>Char</code>, <code>Bool</code>, <code>Int</code>) start with capital letters. But polymorphic types start with lower case letters. We can use longer names for polymorphic types, but the usual is to use single letters (i.e., <code>a</code>, <code>b</code>, <code>c</code>).
</div>

This "`first`" function we just created actually comes with Haskell, but it's named `fst`! And it comes with its counterpart: `snd`!:


```haskell
:t fst
:t snd

fst (1,2)
snd (1,2)
```

`a` and `b` are type variables, meaning they can be of any type. And no matter the type, the value returned by `first` has to be of the same type as the first element of the pair (because they are both of type `a`).

By using type variables, we can use the `first` function with pairs of any type (polymorphic values)!

Notice that `a` and `b` both CAN be of any type AND different types from each other. But they don't HAVE to be. You can use `first` on a tuple with values of the same type: `('a','b') :: (Char, Char)`.

Another example of polymorphic function is the `head` and `tail` functions.

You can use `head` to get the first element of the list and `tail` to get all the elements of the list *except* for the first one.


```haskell
list = [1,2,3,4]
list

:t head
head list

:t tail
tail list
```

We don't care about the specific types. We're just extracting an element. So, the parameter is a polymorphic list (a list of any type, let's call it `[a]`). And the result has to be an element of the same type as the elements on the list. That's why it has to be `a`.

Now that we're familiar with all these types, let's have some fun with lists! (We'll leave fun with tuples after we learn pattern matching. It'll be more fun that way.)

## Fun with lists!

 Every element has an index determined by its position inside the list — starting at 0 (zero).

We use the `!!` operator to access a specific element inside a list by using its index:


```haskell
:t (!!)
"abc" !! 1         
[12,13,16,18] !! 3 
```

Tuples don't have indices, so one cannot easily extract elements of tuples like this. But, we can use `fst` and `snd` for pairs and pattern matching for longer tuples. (See pattern matching lesson to know how.)

Lists can be defined by a range: 


```haskell
[3..22]
```

And we can also specify a step between the items of the range:


```haskell
[3,5..22]
['a','c'..'z']
```

The result of the first expression will hold all elements starting from `3` with a step `2 = 5 - 3` that doesn't exceed `22` (if the last element doesn't fit in a defined step pattern, it will be omitted from the result).

The result of the second expression will hold every other lower-case letter from the alphabet.

It's important to notice that you can only specify one-step size!

If the increment is negative, the elements will be listed in deceasing order:


```haskell
[17,14..3]
```

You can also use ranges to make infinite lists by just not specifying an upper limit.


* `[1..]` is the infinite list $[1,2,3,4,5,...]$.



* `[1,3..]` is the infinite list $[1,3,5,7,9,...]$. 

Now, if we just evaluate the list by itself, the program will run forever (or until it crashes). So, infinite lists are usually used as part of expressions.

We also have the function `take` returns a list containing the first `n` elements in a (potentially infinite) list `l`.


```haskell
:t take

take 3 ['x'..]

take 20 [1,3..]

take 7 [5,3..]  
```

We use the *cons* operator (denoted by a colon `:`) to prepend an element: 


```haskell
:t (:)
2 : [3,4,5]
```

 And we use the **concatenation** `++` operator to put two lists together:


```haskell
:t (++)
[1,3,7,9] ++ [3,3,1]
```

Notice that `++` is a function that takes two lists, and `:` is a function that takes an element and a list.

**Warning** Repeated usage of the `++` operator on long lists (even if you append a singleton list to a list, for instance: `[1,2,3] ++ [4]`), forces Haskell has to **walk through the whole list** on the left side of `++`. Therefore, putting something at the end of a list that's fifty million entries long, is going to take a while! However, putting something at the beginning of a list using the cons operator `:` is instantaneous! 

Among many other useful functions defined for lists, we briefly mention the following:

* `length` takes a list and returns its length;

* `null` checks if a list is empty;

* `sum` takes a list of numbers and returns their sum;

* `elem` takes an element `x` and a list of elements `l` of the same type and checks if `x` is an element of the list `l`.


```haskell
length [2,4,5,6,7]

null [2]

sum [-1,0,1,6,-5,-1]

5 `elem` [6,3,5,7,5]
```

That's it about lists for now. But we'll keep learning more about them through the course!

### Joining and breaking text

There are situations when what you want to do with your list is specifically a text-related thing. Haskell has specific functions for that out of the box.

For example:

* `words :: String -> [String]`    breaks a `String` up into a list of words, which were delimited by white space.

* `unwords :: [String] -> String`  is an inverse operation to words. It joins words with separating spaces.

* `lines :: String -> [String]`    splits the argument into a list of lines, with new line characters (`\n`) serving as separators.

* `unlines :: [String] -> String`  creates a  `String` from an array of strings, appending new line characters (`\n`) between original strings. 


```haskell
words "To be or not to be?"

lines "How are you doing? \n I'm fine, how about you?"
```
