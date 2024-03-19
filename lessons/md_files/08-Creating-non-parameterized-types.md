# Creating Non-parameterized Types

We already covered what types are and why they are useful in previous lessons. So, in this one, we'll learn how to create our own types.

## Outline
* **Type synonyms**
	* How to define them
	* Why use them
* **New types with `data`**
	* Creating types
	* Using types
	* Value Parameters
* **Record syntax**

## Type Synonyms

Early on, when learning about `Strings` in Haskell, you found out that `String` is syntactic sugar for `[Char]`. This means that `String` and `[Char]` are *equivalent* and you can use them *interchangeably*.

That's because `String` is a type synonym for `[Char]`.

### How to define Type Synonyms

To define a type synonym, you use the `type` keyword, followed by the new name for the type and to which pre-existent type is equal to.

```haskell
type String = [Char]
```

You can name the type synonym however you want, as long as it starts with a capital letter.

<div class="alert alert-block alert-warning">
When you define a type synonym, you're not creating a new type! You're telling Haskell that an existing type can be referred with a different name (a synonym)!
</div>

### Why use Type Synonyms

Why would you add more complexity without adding more functionality?

Because type synonyms allow us to convey more information! Let's see an example.

Imagine you started working with a library that allows you to create monetary transactions.

You want to create a new transaction, so you take a look at the type signature of the function that you need to use:

```haskell
generateTx :: String -> String -> Int -> String 
```

Not an extremely useful signature. You could infer that the `Int` is the value to transfer, but what are those `Strings`? And what does that `String` that it returns contain?

Now, compare that type signature with this one:

```haskell
generateTx :: Address -> Address -> Value -> Id
```

Clearly, the second signature transmits the context way better!
The first two parameters are addresses, the third one is the value of the transaction, and it looks like it returns the id of the transaction. 

All that just from the type signature. The difference? Just a few type synonyms.

Let's see what we did to improvie the context so much. Starting by recreating the function called `generateTx` that will take the addresses and value of a transaction and generates a id for it:


```haskell
generateTx :: String -> String -> Int -> String 
generateTx from to value = from ++ to ++ show value
```

Now, we just need to add some type synonyms and sustitute them in the signature:


```haskell
type Address = String
type Value = Int
type Id = String

generateTx :: Address -> Address -> Value -> Id
generateTx from to value = from ++ to ++ show value
```

Super easy! And if you want to check what does the `Address`, `Value`, or `Id` types are, you can open GHCi, load the file, and check its info:


```haskell
:i Address 
```




<div style='background: rgb(247, 247, 247);'><form><textarea id='code'>type Address :: *
type Address = String
  	-- Defined at <interactive>:1:1
</textarea></form></div><script>CodeMirror.fromTextArea(document.getElementById('code'), {mode: 'haskell', readOnly: 'nocursor'});</script>


And, of course, we can build on top of previous type synonyms to create more complex types. Here's an example:


```haskell
type Name = String
type Address = (String, Int)
type Person = (Name, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Person
:t bob
:t fst bob
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
</style><span class='get-type'>bob :: Person</span>



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
</style><span class='get-type'>fst bob :: Name</span>


Having type synonyms is cool and all, but they are just different names for the same thing. What if we need to create a brand new type? `data` to the rescue!

## Defining new types with `data`

We can create new types like this:


```haskell
data PaymentMethod = Cash | Card | Cryptocurrency

data Color = Red | Green | Blue

data Bool = True | False      -- Real definition of Bool

data Ordering = LT | EQ | GT  -- Real definition of Ordering
```

We start with the `data` keyword. Then, the part before the equal sign is our new type name and the part after the equal sign are **value constructors**.

Value constructors specify the different **values** that the type can have.

In this context, the `|` (pipe sign) is read as "or". So we can read the firs type as:

> The type `PaymentMethod` can have a value of `Cash`, `Card`, or `Cryptocurrency`.

<div class="alert alert-block alert-warning">
<b>Warning:</b> The type name and the value constructors must start with an uppercase letter!
</div>

## Using our new type

And now, how can we use this new type?

By using its values! For example, let's add a payment method to our person:


```haskell
type Name = String
type Address = (String, Int)

data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

type Person = (Name, Address, PaymentMethod)

bob = ("Bob Smith", ("Main St.", 555), Cash) :: Person
bob
```


    ("Bob Smith",("Main St.",555),Cash)


<div class="alert alert-block alert-info">
<p>We'll be adding <code>deriving (Show)</code> at the end of our data declarations.</p>
<p>By adding this, Haskell will automatically make that type an instance of the <code>Show</code> type class. Allowing us to print them on the terminal.</p>
<p>We'll explain how this works in detail in the "Creating type classes and Instances" lesson.</p>
</div>

And, of course, we can check its properties using the `:i` command in ghci:


```haskell
:i PaymentMethod
```




<div style='background: rgb(247, 247, 247);'><form><textarea id='code'>type PaymentMethod :: *
data PaymentMethod = Cash | Card | Cryptocurrency
  	-- Defined at <interactive>:3:1
instance [safe] Show PaymentMethod -- Defined at <interactive>:3:61
</textarea></form></div><script>CodeMirror.fromTextArea(document.getElementById('code'), {mode: 'haskell', readOnly: 'nocursor'});</script>


We can pattern match for its values:


```haskell
howItPays :: Person -> String
howItPays (_, _, Cash) = "Pays in cash"
howItPays (_, _, Card) = "Pays with card"
howItPays (_, _, Cryptocurrency) = "Pays with cryptocurrency"

howItPays bob
```


    "Pays in cash"


And use it like any other type.

But that's just the tip of the iceberg. What should we do if we need more than a few values?

What if, for example, I want a type to represent a shape that could be any circle or any rectangle?

We could start by defining something like:


```haskell
data Shape = Circle | Rectangle
```

But the thing is, this isn't of much use.

I want to be able to do stuff with these values, like calculating perimeters and areas. And I can't do that without the actual properties of the shape!

No problem at all! We can just pass some parameters to the constructors!

### Value Parameters

Let's think about what do we need to represent any circle or rectangle:
* To define a circle, we need its radius. So just one numeric value.
* To define a rectangle, we need the length of its two sides. So two numeric values.

To translate those requirements to code, the only thing we need to do is to add other types as arguments for our value constructor when defining the type, like this:


```haskell
data Shape = Circle Float | Rectangle Float Float
```

And this is an example of the famous **algebraic data types** everyone talks about. One of the many properties of Haskell.

They are called "Algebraic" because we can create new types by combining previous ones either by alternation (`A | B`, meaning `A` or `B` but not both) or by combination (`A B`, meaning `A` and `B` together).

And how does this combination works? If we check the type of the `Circle` constructor:


```haskell
-- data Shape = Circle Float | Rectangle Float Float
:t Circle
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
</style><span class='get-type'>Circle :: Float -> Shape</span>


We see that **`Circle` is a function!!** A function that takes a value of type `Float` and returns a value of type `Shape`! So, to obtain a value of type `Shape`, all we have to do is pass its radius:


```haskell
smallCircle = Circle 3

hugeCircle = Circle 100

:t smallCircle
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
</style><span class='get-type'>smallCircle :: Shape</span>


And it's the same for `Rectangle` values:


```haskell
-- data Shape = Circle Float | Rectangle Float Float
:t Rectangle
```

`Rectangle` is a function that takes two values of type `Float` and returns a value of type `Shape`. So, to obtain a rectangle of type `Shape`, all we have to do is pass the lengths of its sides:


```haskell
rect1 = Rectangle 10 5

rect2 = Rectangle 256 128

:t rect1
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
</style><span class='get-type'>rect1 :: Shape</span>


That's it! We created some values of our new `Shape` type. Now let's use them!

We can define a function that calculates the area of any value of type `Shape` like this:


```haskell
area :: Shape -> Float
area (Circle r) = pi * r^2        -- We pattern match on value constructors
area (Rectangle l1 l2) = l1 * l2

area smallCircle
area rect2
```


    28.274334



    32768.0


Now we're talking! We just created a really useful type!

But I'm not done with these shapes yet. I want more! I want to add colors! And points in 2D space that tell you the position of the center of the shape!

For that, we could do something like this monstrosity:


```haskell
data Shape
  = Circle (Float, Float) Float String
  | Rectangle (Float, Float) Float Float String
```

Where we add the points in space as tuples of `Float` values and colors as a `String` value.

We could easily redefine the `area` function for this new type like this:


```haskell
area :: Shape -> Float
area (Circle _ r _) = pi * r^2
area (Rectangle _ l1 l2 _) = l1 * l2
```

But then, if we want to extract specific fields of the `Shape` type, we have to create a custom function for each and every one of them:


```haskell
color :: Shape -> String
color (Circle _ _ c) = c
color (Rectangle _ _ _ c) = c

point :: Shape -> (Float, Float)
point (Circle p _ _) = p
point (Rectangle p _ _ _) = p

--- Etc...
```


```haskell
type Point = (Float,Float)
type Radius = Float
type Width = Float
type Height = Float
type Color = String

data Shape
    = Circle Point Radius Color
    | Rectangle Point Width Height Color
```

The actual type is way more readable. I'll give you that.

But that's a lot of type synonyms to improve the understanding of the signature. And on top of that, it doesn't solve the other—more pressing—issues!

But don't worry, Haskell has our backs! Enter the record syntax!

## Record Syntax

***Record syntax* is an alternative way of defining data types that comes with a few perks.**

We'll start with an easier example, and then we will fix our `Shape` type.

Let's say we want to create an `Employee` data type that contains the employee's name and years of experience.

Without *record syntax*, we would create it like this:


```haskell
data Employee = Employee String Float
```

<div class="alert alert-block alert-info">
In this case, because the type has only one value constructor, it's usual to use the same name as the name of the type. It's not like there's anything special about it, it's just convention.
</div>

But with *record syntax*, we can create it like this:


```haskell
data Employee = Employee { name :: String, experienceInYears :: Float } deriving (Show)
```

As you can see: 
* Record syntax value constructors have their parameters—that we call fields—surrounded by curly brackets.
* Each field has a name that starts with a lowercase letter followed by its type.
* And the fields are separated by commas.

Ok, we have our new `Employee` type. Now let's use it.

We can create values like this:


```haskell
richard = Employee { name = "Richard", experienceInYears = 7.5 }

:t richard
richard
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
</style><span class='get-type'>richard :: Employee</span>



    Employee {name = "Richard", experienceInYears = 7.5}


We provide the constructor, and between its curly brackets, we specify the name of each field with its corresponding value. In any order!

Right off the bat, the resulting data type is easier to understand, and the `Show` instance is more explicit when we print it. The only downside is that we need to write all that extra code... or do we?


```haskell
matt = Employee "Matt" 5
matt
```


    Employee {name = "Matt", experienceInYears = 5.0}


You can also create new values of the `Employee` type by passing the parameters of the value constructors in the same order as its definition to get the same final result! No extra code is needed. 

And that's not even making it to the top 3 best perks! Another one is that we can update the value of a record by creating a new value from a previous one and only specifying the fields that changed, like this:


```haskell
newMatt = matt { experienceInYears = 6 }
newMatt
```


    Employee {name = "Matt", experienceInYears = 6.0}


An even better one is that **it automatically generates functions to look up fields in the data type!**


```haskell
:t name
name richard

:t experienceInYears
experienceInYears richard
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
</style><span class='get-type'>name :: Employee -> String</span>



    "Richard"



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
</style><span class='get-type'>experienceInYears :: Employee -> Float</span>



    7.5


Because we have two fields (the `name` and `experienceInYears` fields), we get, for free, two functions of the same name that take a value of type `Employee` and return the value of the field.

Now, if we want, for example, to calculate the combined experience of your team, you could do something like:


```haskell
team = [Employee "John" 4, Employee "Josh" 2, Employee "Matthew" 7]

combinedExp :: [Employee] -> Float
combinedExp = foldr (\e acc -> experienceInYears e + acc) 0

combinedExp team
```


    13.0


Really convenient! And there's more! But before revealing a final awesome property of record syntax, let's use this new power and redefine the unintelligible `Shape` type.

As you recall, without record syntax, the data type definition was this one:


```haskell
data Shape
  = Circle (Float, Float) Float String
  | Rectangle (Float, Float) Float Float String
```

Well, with record syntax is this one:


```haskell
data Shape
  = Circle
      { position :: (Float, Float)
      , radius   :: Float
      , color    :: String
      }
  | Rectangle
      { position :: (Float, Float)
      , width    :: Float
      , height   :: Float
      , color    :: String
      }
  deriving (Show)
```

As you can see, all we have to do is to replace the constructor's parameters with record fields, and we can use the data type the same as we did with the `Employee` type.

We can create values using regular and record syntax, and we can update values by specifying only the fields we need to change:


```haskell
circ = Circle { position = (1, 2), radius = 6, color = "Green" }
:t circ
circ

rect1 = Rectangle (9, 3) 7 3 "Yellow"
:t rect1
rect1

rect2 = rect1 {width = 12}
:t rect2
rect2
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
</style><span class='get-type'>circ :: Shape</span>



    Circle {position = (1.0,2.0), radius = 6.0, color = "Green"}



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
</style><span class='get-type'>rect1 :: Shape</span>



    Rectangle {position = (9.0,3.0), width = 7.0, height = 3.0, color = "Yellow"}



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
</style><span class='get-type'>rect2 :: Shape</span>



    Rectangle {position = (9.0,3.0), width = 12.0, height = 3.0, color = "Yellow"}


And, of course, we can easily extract the values we need with our shiny new automatically defined functions:


```haskell
position circ

color rect2
```


    (1.0,2.0)



    "Yellow"


I know, I know, I'm showing you the same thing again but with a different type. I don't want to bore you, so let's see something else that comes with records.

Let's use pattern matching to redefine the function to calculate the area of the shape for our new record data type.

Even if we're using record syntax, we can still pattern-match like we always did:


```haskell
area :: Shape -> Float
area (Circle _ r _) = pi * r ^ 2
area (Rectangle _ w h _) = w * h

area circ
area rect1
```


    113.097336



    21.0


We don't lose the ability to do that.

But, thanks to records, now we have a special pattern-matching syntax!:


```haskell
area :: Shape -> Float
area Circle {radius=r} = pi * r^2
area Rectangle {width=w,height=h} = w * h

area circ
area rect1
```


    113.097336



    21.0


We pattern match on record-syntax value constructors by writing the constructor's fields between curly brackets and binding them to a variable on the right side of the field's equal sign.

What's interesting is that we only match the patterns of the fields we need to use. And this gives us another fantastic perk of record syntax. If we add another field to the data type, we don't need to change any of our previous functions! Because we don't take into account unused fields in our pattern matching!

Awesome, right?

Record syntax is especially useful when you have a data type with maybe dozens of fields. Like a type that contains the settings of an application. Or one that contains all the fields of a survey. 

It allows you to use the type without the need to remember which value was what (because they are all named) and allows you to update and reference specific fields, ignoring the rest. So, if you change your type in the future, only the values and functions that use the changed field are affected. If any.

# That's it for today!

Ok, that's it for today. In the next lesson, we'll build on top of this one to create more complex types. So make sure to do the homework, and I'll see you at the next one!
