# Introduction to


![](https://www.haskell.org/img/haskell-logo.svg)

## Outline
* What is Haskell?
* Functional programming languages
	* Function composition
* Explicit effects (Pure)
* Basic syntax
	* Indenting and Commenting
	* Defining and using functions
* Haskell Type system
* Laziness
* Tools: GHC (GHCi), Cabal, Stack

## How to use JupyterLab
* Each lesson is a Jupyter notebook.
* Each Jupyter notebook is a series of cells.
* To execute a cell, click ⇧⏎ (Shift + Enter).
* You can play around with the code inside the cells.
* Once you close the tab, every change will be lost.

## What is Haskell?

We'll go over each property of Haskell individually and answer this question at the end of the lecture.

##  Functional programming languages

Haskell is a functional programming language.

In imperative programming languages, function definitions are a sequence of imperative statements.


In functional programming languages, function definitions are **trees of expressions that map values to other values**.


**Programs are constructed by *applying* and *composing* functions**. 

### Function composition

**Function composition is the act of *pipelining* the *result* of one function, to the *input* of another, creating an entirely new function**

Like the usual composition of functions in mathematics, the **result of each function is passed as the argument of the next**, and the result of the last one is the result of the whole.

For example, suppose we have two functions $f$ and $g$:

$$y = f(x)$$
$$z = g(y)$$

Composing them means we first compute $f(x)$ to get $y$, and then use $y$ as an argument to compute $g(y)$ to get $z$.

Creating a function that goes from $x$ to $z$:

$$z = g(f(x))$$

And that's how we can create **arbitrarily complex functions by composing simple functions.** 

For example, if we have:

* A function that takes a spreadsheet and returns the list of players it contains.
* A function that takes a list of players and returns the same list sorted by scores.
* And a function that takes a list of players and returns the first 3.

We could create **a single function that takes a spreadsheet and returns the 3 best players** by composing those three functions.

Also, Haskell has explicit effects (also called pure 👼)!

## Explicit effects (purely functional)

Purely functional programming languages treat **all computations as the evaluation of mathematical functions**.

In mathematics, the expression $y = x + 1$ means that the value of $y$ is a function that depends on $x$.

For a specific $x$, the value of $y$ will always be the same.

No matter if you're in Italy or Spain, if it's 1994 or 2022, or if you have other equations in the notebook. $y$ will care about the value of $x$ and nothing else. 

In purely functional programming languages, pure functions depend **only on their arguments** and **don't interact with any global or local state**. (This is called having "no *side effects*.")

This means that, **for a specific input, a function will always return the same value. Every time.**

It sounds like a bad idea, but if you think about it, it has some extremely convenient consequences:

* It lets you easily deduce and prove that a function is correct.
* In Haskell, one can always “replace equals by equals”, just like you learned in algebra class.
* Makes your code significantly less error-prone.
* It's easier to do parallel/concurrent computing. (If there is no data dependency between two pure expressions, then they can be performed in parallel, and they cannot interfere with one another.)
* The list goes on...

**Haskell works as a pure language, but allows for side effects (network communication, I/O, etc.) by explicitly tagging them in the type system.** We'll see how in future lessons. (This is called having "*explicit effects*").

Before continuing with more properties, let's see how Haskell actually looks like.

## Basic syntax

### Commenting the code


```haskell
-- Use double dash to comment within a single line of code.

{-
Use curly bracket with a single dash to
  open and close
multi-line comments.
-}
```

### Indentation

**Haskell is indentation sensitive**. Which means that spaces, tabs, and newlines matter.

The golden rule is:


<p><blockquote>Code which is part of some expression should be indented further in than the beginning of that expression (even if the expression is not the leftmost element of the line).</blockquote><p>

We'll look at examples in future lessons.

### Defining functions

Haskell being a functional programming language means that you're going to write plenty of functions. So,  that's where we'll start.

This is an expression to define a function that checks if a number is greater than 18:


```haskell
greaterThan18 x = x > 18
```

* `greaterThan18` is the name of the function. Choose a name that makes it easy to know what it does.
* `x` is a parameter.
* The `=` operator assigns the `x > 18` expression to the `greaterThan18` name.

To the left of the `=` sign, we write the function's name and parameters. And to the right, the expression that'll be contained by this function.

### Using functions

To use the `greaterThan18` function, we just have to write the name, a space, and write a number:


```haskell
greaterThan18 3
```

The function is executed, Haskell replaces all the `x` with `30`, and `greaterThan18 30` becomes `30 > 18`. Then, it evaluates the expression, returning `True`.

### More examples


```haskell
-- A function that adds 6 numbers:
add6numbers u v w x y z = u + v + w + x + y + z
add6numbers  1 2 3 4 5 6  -- 21
```


```haskell
-- A function that calculates the volume of a cylinder
volumeOfACylinder r h = pi * r^2 * h  -- pi represents the number π, and it comes with Haskell
volumeOfACylinder 3 10
```


```haskell
-- A function that takes the temperature in Fahrenheit and returns it in Celsius
fToC x = (x - 32) * 5 / 9
fToC 212  -- 100	
```

### Key points
* Parameters are separated by spaces.
* Everything after the `=` is the function's body.
* The first letter of a function's name has to be lowercase.
* We use parentheses to prioritize calculations, like in math.

## Haskell type system

We will cover Haskell type system in depth in lesson 2. Here you will learn some basics.

Types are **attributes that constraint the values a piece of code can have**. For example, if you indicate that some data is a number, that data could have any of these values:

      32

      9999695939294

      0.5

But, if you try to add a character in there, like this: `6A3` (instead of `63`), the compiler/interpreter will yell at you.

What your compiler/interpreter did just there is called "**type checking**." Some languages have more strongly enforced type checking, some less.

      6A3

### Type checking

**Type checking is the process of verifying and enforcing the constraints of types.**

**What does this mean?** It means that each type has its own constraints (E.g., you can't do math with letters.), and this process checks that those constraints are respected.

**Why would you do this?** To avoid preventable mistakes.

### Dynamically typed languages

If further along in your program, you want to add up some numbers and one of them has a letter, the program wouldn't know what to do, and it would crash on you. Those are preventable mistakes (bugs), and the compiler/interpreter helps you avoid them.

Usually, this is done automatically. But not all languages do this the same way. There are two main distinctions regarding as to WHEN the types are checked: Dynamically typed languages and Statically typed languages.

**Dynamically typed languages check the types at run-time**.

Run-time is the very last thing that you do with a program. It's the stage when you run your program to test it or use it.

Common examples of dynamically typed languages include JavaScript, Python, Objective-C, and PHP.

### Statically typed languages

**Statically typed languages check the types at compile-time**.

Meaning that you'll know if your types are wrong as soon as you compile your program. Which leads to a safer and more optimized code.

Common examples of statically typed languages include Java, C, and C++.

### Haskell type system

**Haskell is statically typed**. And, in Haskell, **every expression has a type**.

But don't worry, you don't have to manually define the types of every expression because Haskell's compiler is very good at **type inference**.

**Type inference allows Haskell to infer the types on its own**.

If you write something like `3 + 4`, Haskell will know that the result of that expression is a number, and it will treat it like it without the need for you to specify the type. (It works with more complicated expressions, too. See previous examples.)

That allows the compiler to **comprehend and reason *quite a lot* about your program**. Providing you with a pretty effective bug-catching assistant.

Even though it's not needed for the compiler, **it's considered good practice to write out the type signature of top-level functions and expressions**. To improve code readability.

If the code is too ambiguous for the compiler to infer the type, it'll ask you to specify the type.

## Laziness

**Haskell is lazy. This means that it won't evaluate expressions until their results are needed**

Examples of Haskell laziness in practice:

* We can use infinite lists.


```haskell
giveMe x = take x [1..] -- [1..] is an infinite list of natural numbers that starts at 1.
giveMe 7
```


* Haskell won't evaluate expressions if they aren't needed


```haskell
cheapComputation = 7 
expensiveComputation = sum [1..10000000] -- sum is a function that takes a list and returns the sum of all the elements. This will crash the kernel.
if cheapComputation > 5 || expensiveComputation > 5 then "Done" else "This won't ever show because expensiveComputation is always bigger than 5"
-- Try running this cell with cheapComputation being bigger and smaller than 5.
-- When cheapComputation > 5, expensiveComputation isn't evaluated because it is not needed.
```

## So, what is Haskell?

#### Haskell is a statically typed, **lazy**, functional programming language with **explicit effects** and functions that look like this:

```haskell
volumeOfACylinder r h = pi * r^2 * h 
```

<div class="alert alert-block alert-info">
<b>Note:</b> Haskell has other important properties (like algebraic data types, type classes, type inference, polymorphism, ...) that we'll cover in future lessons.
</div>

(*Lazy* and *explicit effects* are two of the more unique properties of Haskell. That's why they're in bold.)

## Tools

### A few words on Cabal and Stack

While learning about Haskell, you'll often encounter the Cabal and Stack terms.

**These are systems for managing libraries and programs**. They make it straightforward to work with libraries.

We'll use Cabal in this course, and we'll explain how to use it at a later stage.

### GHC and GHCi

**GHC (Glasgow Haskell Compiler) is a compiler and interactive environment for Haskell**. Using GHC we can:
* Compile programs and execute them like any other app.
* Evaluate Haskell expressions on the fly using the interactive environment provided by the GHC (the GHCi).

To use GHCi, open the terminal in the GitPod remote environment that we've prepared and type `ghci`.

Use `:l relative/path.hs` inside GHCi to load a file and interactively use its contents and `:q` to quit.

**NOTE:** If you want to install GHC and GHCi on your computer you can follown the instructions on www.haskell.org/ghcup/. Instructions are provided for Windows, Mac and Linux.
