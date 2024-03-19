# Conditions and helper constructions

## Outline

* If-then-else expressions.

* Guards

* `let` expressions

* `where`

* Should I use `let` or `where`?

* Things to keep in mind

## If-then-else expressions

Often in your code, you have to make a choice. There are several ways to express conditions. In Haskell, we most commonly use **if-then-else** expressions:

```haskell
if <Condition> 
  then <Expesssion1>
  else <Expesssion2>
```

Where `Condition` is a logical expression that yields `True` or `False`, `Expression1` is the expression used if `Condition` is `True`, and `Expression2` is the expression used if `Condition` is `False`. The function `checkLocalHost` below checks whether the argument is localhost or not and reports it to the user.


```haskell
checkLocalhost :: String -> String
checkLocalhost ip =
    -- True or False?
    if ip == "127.0.0.1"
        -- When the condition is True the answer is
        then "It's localhost!"
        -- Otherwise the condition is False and the answer is
        else "No, it's not localhost."

checkLocalhost "127.0.0.1"
```


    "It's localhost!"


The `checkLocalhost` function is applied to a single argument of type `String` and returns another value of type `String`. The argument is a string `ip` containing the IP address, and the function checks if the string is equal to `"127.0.0.1"`. If the check is successful the function returns `"It's localhost!"`, otherwise it returns `"No, it's not localhost."` 

<div class="alert alert-block alert-info">
    While in imperative programming languages, the <code>else</code> is not mandatory, in Haskell, it is! That's because, in Haskell, every function has to return a value. So, we are obligated to provide a result of the same type for both the <code>then</code> and <code>else</code> cases. 
</div>

## Guards

Now, imagine that we want to do a more complex check. Like checking if this year's birthday has some special meaning. We could use nested if-else statements like this:


```haskell
specialBirthday :: Int -> [Char]
specialBirthday age =
  if age == 1
    then "First birthday!"
    else
      if age == 18
        then "You're an adult!"
        else
          if age == 60
            then "Finally, I can stop caring about new lingo!"
            else "Nothing special"
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning { 
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error { 
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><div class="suggestion-name" style="clear:both;">Use guards</div><div class="suggestion-row" style="float: left;"><div class="suggestion-warning">Found:</div><div class="highlight-code" id="haskell">specialBirthday age
  = if age == 1 then
        "First birthday!"
    else
        if age == 18 then
            "You're an adult!"
        else
            if age == 60 then
                "Finally, I can stop caring about new lingo!"
            else
                "Nothing special"</div></div><div class="suggestion-row" style="float: left;"><div class="suggestion-warning">Why Not:</div><div class="highlight-code" id="haskell">specialBirthday age
  | age == 1 = "First birthday!"
  | age == 18 = "You're an adult!"
  | age == 60 = "Finally, I can stop caring about new lingo!"
  | otherwise = "Nothing special"</div></div>


That's just a mess! Too complicated to both read and write. Luckily, we have guards!

Guards work similarly to if-else statements, but you can have multiple conditions:

```haskell
func arg
  | <Condition1> = <Result1>
  | <Condition2> = <Result2>
  | <Condition3> = <Result3> 
  ...
```

We use the symbol `|` to indicate the beginning of each guard.

<div class="alert alert-block alert-info">
    Notice that there's no <code>=</code> sign after <code>func</code> arguments! That's a common pitfall when writing guards. Don't add that <code>=</code>!
</div>

With guards, we can write the `specialBirthday` function like this:


```haskell
specialBirthday :: Int -> [Char]
specialBirthday age
  | age == 1 = "First birthday!"
  | age == 18 = "You're an adult!"
  | age == 60 = "Finally, I can stop caring about new lingo!"
  | True = "Nothing special"
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning { 
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error { 
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><div class="suggestion-name" style="clear:both;">Use otherwise</div><div class="suggestion-row" style="float: left;"><div class="suggestion-warning">Found:</div><div class="highlight-code" id="haskell">specialBirthday age
  | age == 1 = "First birthday!"
  | age == 18 = "You're an adult!"
  | age == 60 = "Finally, I can stop caring about new lingo!"
  | True = "Nothing special"</div></div><div class="suggestion-row" style="float: left;"><div class="suggestion-warning">Why Not:</div><div class="highlight-code" id="haskell">specialBirthday age
  | age == 1 = "First birthday!"
  | age == 18 = "You're an adult!"
  | age == 60 = "Finally, I can stop caring about new lingo!"
  | otherwise = "Nothing special"</div></div>


That last `True` is there to be a catch-all condition. A condition that always evaluates to `True` because it's literally `True`.

This pattern of adding a last `True` in the last guard is so common that Haskell comes with a variable called `otherwise` that it's equal to `True` (`otherwise = True`) to make for an even more readable guard:


```haskell
specialBirthday :: Int -> [Char]
specialBirthday age
  | age == 1 = "First birthday!"
  | age == 18 = "You're an adult!"
  | age == 60 = "Finally, I can stop caring about new lingo!"
  | otherwise = "Nothing special"

specialBirthday 60
```


    "Finally, I can stop caring about new lingo!"


Now you can easily understand what this expression does with a quick glance!

OK, that's it about conditional evaluations. Now let's see how we can take our function-syntax game up a notch with `let` and `where`!

## `let` and `where`

We use `let` and `where` to store the results of intermediate computations and bind local variables.

 Let's start with `let`!

### `let` expressions

`let` can bind expressions to local variables in the following way:

```haskell
func arg =
    let <BIND_1> 
        <BIND_2> 
    in  <EXPR that uses BIND_1 and/or BIND_2>
```

Where `<BIND_X>` are local bindings accessible throughout the entire `let` expression.

Now, let's create a function that takes two temperatures—one in Celsius and one in Fahrenheit—and returns the hotter one but in Kelvin. Those are quite a few conversions, aren't they?

To go from Fahrenheit to Celsius, we have to first subtract 32 and then multiply by 5/9, like this:

 $tC = (tF - 32) * 5/9$

To go from Celsius to Kelvin, we just need to add 273.16 like this:

$tK = tC + 273.16$

So, if we want to create **a single function** that does all that, we can create something like this:


```haskell
hotterInKelvin :: Double -> Double -> Double
hotterInKelvin c f = if c > (f - 32) * 5 / 9 then c + 273.16 else ((f - 32) * 5 / 9) + 273.16

hotterInKelvin 40 100
```


    313.16


It works, but that's textbook I-have-no-idea-what-I-wanted-to-do-with-that-two-weeks-ago code.

A better approach is using `let` bindings for the intermediate expressions and writing the expression that pulls everything together at the `in` part:


```haskell
hotterInKelvin' :: Double -> Double -> Double
hotterInKelvin' c f =
  let fToC t = (t - 32) * 5 / 9
      cToK t = t + 273.16
      fToK t = cToK (fToC t)
   in if c > fToC f then cToK c else fToK f

hotterInKelvin' 40 100
```


    313.16


Now our code is way more readable and doesn't have all that repeated expressions!

But wait, there's more! We can also use the `where` construction!

### `where`

We can use `where` to bind values to variables in the following way:

```haskell
func arg = <EXP that uses BIND_1 and/or BIND_2>
    where <BIND_1>
          <BIND_2>
```

So, the same `hotterInKelvin` function as before can be expressed with `where` like this:

Where `<BIND_X>` are bindings accessible throughout the entire function body.


```haskell
hotterInKelvin'' :: Double -> Double -> Double
hotterInKelvin'' c f = if c > fToC f then cToK c else fToK f
  where
    fToC t = (t - 32) * 5 / 9
    cToK t = t + 273.16
    fToK t = cToK (fToC t)

hotterInKelvin'' 40 100
```


    313.16


Ok, they both seem to do the same thing. So, why bother having both? Couldn't we just choose to use one of them?

Well, there are plenty of cases where they are interchangeable. In those cases, you can choose whichever you like the most. But they also have their limitations and strengths.

### Should I use `let` or  `where`?

`let` expressions are convenient whenever we want to split complex expressions into smaller building blocks that you combine into a final expression.

For example, imagine you wish to calculate the volume of a house. We could simplify the problem like this:

A house is a cube with a pyramid on top (the roof). So, to find its volume, we need to calculate the volume of the cube and the volume of the pyramid and add them together:


```haskell
houseV side roofH = let cubeV = side ^ 3
                        pyramidV = (side ^ 2) * roofH / 3
                    in  cubeV + pyramidV
                    
houseV 3 1
```


    30.0


We create the `cubeV` and `pyramidV` building blocks inside the `let` block, and then we use them inside the `in` expression.

Besides the clarity of the syntax, another advantage is that if the final expression later becomes more complicated (for example, we add a chimney to the house), we just need to add another binding and use it in the final expression!:


```haskell
houseV side roofH = let cubeV = side ^ 3
                        pyramidV = (side ^ 2) * roofH / 3
                        chimneyV = (0.5 ^ 2) * roofH
                    in  cubeV + pyramidV + chimneyV
                    
houseV 3 1
```


    30.25


On the other hand, `where` expressions are convenient whenever we want to scope bindings over several guarded equations.

Because we can't access `let` bindings across all guards, but with `where` bindings, we can!!
For example:


```haskell
analyzeCylinder :: Float -> Float -> String
analyzeCylinder diameter height
       | volume < 10 = "The cylinder is a glass."
       | volume < 100 = "The cylinder is a bucket."
       | volume < 1000 = "The cylinder is a tank."
       | otherwise = "What in the world is that huge thing?!"
    where
        volume = pi * diameter^2 * height / 4

analyzeCylinder 15 6
```


    "What in the world is that huge thing?!"


As you can see, we defined the `volume` binding inside the `where` block, and then we accessed it on every guarded expression!

And finally, the main difference between the two is that `where` bindings are declarations bounded to a surrounding syntactic construct. Meaning, they can only be used in specific places (like inside a function body). But `let` introduces an expression, so it can be used wherever an expression can be used. For example:


```haskell
-- Seconds in a day
24 * (let seconds = 60 in seconds * 60) 

-- The volume of a rectangular prism (we can separate expressions by semicolons to have them in the same line)
let s1 = 10; s2 = 20; s3 = 30; in s1*s2*s3 
```


    86400



    6000


In all the cases where you could use one or the other, pick the one that feels right for the situation or your style. It takes some practice to appropriately choose which one to use, and there's also the programmer's preference. So don't worry too much about it. 

### Things to keep in mind

Expressions defined with `where` are not accessible outside that function body.


```haskell
fToK t = 273.16 + fToC t
    where fToC t = (t - 32) * 5 / 9
    
fToC 60
```


    <interactive>:1:1: error:
        • Variable not in scope: fToC :: t0 -> t
        • Perhaps you meant ‘fToK’ (line 1)


Expression introduced in a `let` expression exist only within that `let` expression.

For example, this function takes your name and last name and returns your initials:


```haskell
initials :: String -> String -> String
initials name lastName = if name == "" || lastName == ""
                         then "How was your name again?"
                         else let x = head name
                                  y = head lastName
                              in [x] ++ "." ++ [y] ++ "."

initials "Richard" "Feynman"
```


    "R.F."


The expressions `x` and `y` are only available inside that `let` expression. If you tried to use them inside `if` or `then`, they would be outside the scope, and it would not compile.

## Summary

In this lesson, we've discussed:

* If-then-else statements, and why you always have to define the else case.

* How to use guards to avoid nested if-else statements.

* How to use `let` and `where` to store the results of intermediate computations, bind local variables, allow for cleaner code, and avoid repeating yourself.
