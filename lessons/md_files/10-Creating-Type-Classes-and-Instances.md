# Creating Type Classes and Instances

## Outline

* Overloading
* Steps to create Type Classes and Instances
* The `Eq` type class
	* Defining the Type Class
    * Defining multiple instances
	* Improving our `Eq` type class with mutual recursion (and MCD)
	* Defining an instance for a parameterized type.
* The `WeAccept` Type Class
* The `Container` Type Class
* Exploring `Ord` type class (Subclassing)
* Deriving
    * Deriving can go wrong


## Overloading

Before learning what Overloading is, let's learn what the word "date" means. 

DATE:

What does "date" mean? If I'd say that you have only one chance to answer, and I'll give you $100 if you answer correctly, the intuitive answer is: "It depends!"

If you're saying: "What is your date of birth?," then it means:

1. The time at which an event occurs.

If you're saying: "Joe took Laura out on a date.", then it means:

2. A social engagement that often has a romantic character (unless Joe gets friend-zoned).

If you're saying: "I'll want to date a fossil," I want to believe you're not referring to a romantic social engagement but to:

3. The act of estimating or computing a date or chronology.

And if you look up the word, "date" is also the name of a fruit and has even more definitions!

In programming, we would say that the word "date" is overloaded. Because it has multiple definitions for the same name. 

The word "overloading" is overloaded itself.

OVERLOADING:

In everyday context, it usually means:

1. To put too large a load on or in (something).

In a regular programming context, it means:

2. Having multiple implementations of a function with the same name.

How this work in practice depends on the language. For example, some languages, like JavaScript, don't support overloading. So you can not do it. And in others, like C++, you can create multiple functions with the same name, and the compiler will choose which definition to use based on the types of the arguments.

In Haskell, "overloading" means:

3. Having multiple implementations of a function or value with the same name.

Of course, Haskell had to step up the game. In Haskell, overloading is not restricted to functions. Values can be overloaded too. For example:

- The literals `1`, `2`, etc. are overloaded because they could be interpreted as any numeric type (`Int`, `Integer`, `Float`, etc.)

- The value `minBound` is overloaded because, for example, when used as a `Char`, it will have value `'\NUL'` while as an `Int`, it's `-2147483648`. 

- The equality operator (`==`) works with many types, each with its own implementation.

- The function `max` also works with many types, each with its own implementation.

The first two are overloaded values, and the last are overloaded functions. So, we've been using overloaded functions and values all along. The question is: How do we get those in the first place? Well, the mechanism that allows overloading in Haskell is Type Classes.

## Steps to create Type Classes and Instances

In the "introduction to type classes" lesson, we saw the utility of type classes. It basically boils down to having functions that can be used by many different types while having the safety that they only take the ones they can work with. So, if you create a function that takes two numbers and adds them together, that function works with all numeric types while also having the compiler stop you when trying to give it a non-numeric type.

Type classes are a pretty unique feature–not many programming languages have them. But the good thing is that they're surprisingly easy to use!

When creating our own type classes, we only need two things.

1. Create a Type Class stating some behaviors.


2. Make a Type an instance of that Type Class with the implementation of those behaviors for that specific type.

That's it.

Practice makes perfect, so let's learn by doing. We'll start by redefining the `Eq` Type Class.

## The `Eq` type class

The `Eq` type class comes with Haskell, so you don't have to define it. But let's say that we live in a world where the `Eq` type class doesn't exist, and every type has its own function to check for equality. Because of that, you have to learn a bunch of different functions that all do the same: Checking for equality.

But, as Lennon said, imagine. While living in that horrible world, imagine all the types living in peace and using the same function. It's easy if you try. You may say I'm a dreamer, but let's do it anyway!

We can define the `Eq` type class like this:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

In the first line, we start with the `class` keyword to indicate we're creating a type class. Followed by how the type class will be called (`Eq`). Then, we write a type variable (`a`) that represents any type that will be made an instance of this type class in the future. So, it's like a placeholder. And finally, we use the `where` keyword to start the block where we define the behaviors of our newly created type class.

And now comes the cool part. We have to define the behaviors. To do that,  we write the name and type of the functions or values we need. In this case, we define the behaviors to be the `==` function–to check if two values are equal and the `/=` function–to check if two values are different.

We also indicate that both take two values of the type `a` we specified as the parameter of the type class and return a `Bool`. `True` if the condition passes, and `False` if it doesn't.

And done! We have our `Eq` type class ready to go! This means we have the name and types of the two functions that the `Eq` type class provides. We don't have the definitions here because each type will have its own definitions. And those definitions are provided when defining an instance for the type class.

### Defining an instance for the `Eq` type class

First, we need a type, so let's define one for the payment methods a customer can use in our app:


```haskell
data PaymentMethod = Cash | Card | CC -- CC stands for Cryptocurrency

type User = (String, PaymentMethod)
```

And if we want, for example, to check if two users have the same payment method, we could write a function like this one:


```haskell
samePM :: User -> User -> Bool
samePM (_, pm1) (_, pm2) = pm1 == pm2  -- Won't work!
```


    <interactive>:2:28: error:
        • No instance for (Eq PaymentMethod) arising from a use of ‘==’
        • In the expression: pm1 == pm2
          In an equation for ‘samePM’: samePM (_, pm1) (_, pm2) = pm1 == pm2


But, the compiler won't let you use this code! And it tells us why:

```
No instance for (Eq PaymentMethod) arising from a use of ‘==’
In the expression: pm1 == pm2
```

We're using `==` in the expression `pm1 == pm1`. But, because `==` is a behavior of the `Eq` type class, and our new `PaymentMethod` type is not an instance of the `Eq` type class! So it doesn't have the implementations of `==` and `/=` to use. To fix this, we'll make it an instance!

```haskell
-- class Eq a where
--   ...

instance Eq PaymentMethod where
  -- Implementations for Eq behaviors specific to PaymentMethod
```

To create an instance, we use the `instance` keyword followed by the name of the type class we want to make an instance for, the type that will be an instance of that type class, and the `where` keyword. Then, inside that block, we implement the functions defined in that type class.

As you can see, because now we're creating an instance for a type, we replace the type variable (`a`) we had in the type class definition with our specific type (`PaymentMethod`).

And because we're creating an instance for the Eq type class, we need to implement the `==` and `/=` functions. So we'll do just that:


```haskell
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- data PaymentMethod = Cash | Card | CC

instance Eq PaymentMethod where
  -- Implementation of ==
  Cash == Cash = True
  Card == Card = True -- Same as: (==) Card Card = True
  CC == CC = True
  _ == _ = False
  
  -- Implementation of /=
  Cash /= Cash = False
  Card /= Card = False
  CC /= CC = False
  _ /= _ = True
```

And that's it! That's how you define a type class and make a type an instance of it! Now, `PaymentMethod` can freely use the `Eq` behaviors (`==` and `/=`):


```haskell
Card == Cash
CC /= Card
```


    False



    True


And the previous function will work now:


```haskell
samePM :: User -> User -> Bool
samePM (_, pm1) (_, pm2) = pm1 == pm2  -- It's alive! 

samePM ("Rick", CC) ("Marta", CC)
```


    True


### Improving our `Eq` type class with Mutual Recursion

Our work is technically done. We have our type class and our instance. But there's a property of the functions we just defined that we're not taking advantage of.

If two values are equal, that means they are not different, and if they are different, that means they are not equal. So, we know that for each pair of values, `==` and `/=` will always give us the opposite `Bool` value.

We're on our way to becoming epic Haskell developers, and epic Haskell developers can do better than that. So let's use this knowledge to improve our type class and instance! Starting by redefining the `Eq` type class like this:

```haskell
class Eq a where
  (==), (/=)  :: a -> a -> Bool
  x /= y      = not (x == y)
  x == y      = not (x /= y)
```

**Which is how `Eq` is actually defined in Haskell!**

Let's analyze this code. Because both functions have the same type, we can specify them in a single line. And, yes, we're also writing function definitions inside the type class. We can do that as long as they are type-agnostic because they have to work with all types.

Looking at the definitions in more detail, we see we're using the `not` function. The `not` function takes a boolean and returns its opposite.

So, in the third line, we're saying that the result of applying `/=` to `x` and `y` is the oposite (`not`) of the result of applying `==` to the same `x` and `y`. And in the fourth line, we're saying that the result of applying `==` to `x` and `y` is the oposite (`not`) of the result of applying `/=` to the same `x` and `y`.

This is called mutual recursion because both functions are defined in terms of each other. By defining `==` and `/=` as the opposite of each other, Haskell can infer the behavior of one from the other. 

And, of course, like any other recursion, it needs a base case to know when to stop the recursion! And that's what we provide when implementing an instance! For example, let's redefine the PaymentMethod instance for this new type class:


```haskell
instance Eq PaymentMethod where
  Cash == Cash = True
  Card == Card = True
  CC == CC = True
  _ == _ = False
```

That's it! Because now the compiler can infer the value of one function with the other, we don't need to implement both `==` and `/=`. We can implement the more convenient one and call it a day!

This is called **minimal complete definition**. Because it's the minimum you have to implement to get a fully functional instance. You can take advantage of this by checking the minimal complete definition of any type class using `:i <type class>` and implementing only those behaviors. For example, if you run `:i Eq` in GHCi, you'll get:

```haskell
type Eq :: * -> Constraint -- Eq takes a concrete type and returns a Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}

-- ... and a bunch of instances.
```

In this line:

```haskell
{-# MINIMAL (==) | (/=) #-}
```

It says that to have the *minimal complete definition* of the type class, you have to implement either `==` OR `/=`.

In the real world, almost all types are instances of the `Eq` type class. But remember, we're in a parallel universe where you're a visionary creating the `Eq` type class to make the world a better place. So, if we stop here, the `==` and `/=` functions wouldn't be overloaded! Because they would have only the definition for `PaymentMethod`.

But there's a reason you decided to create this `Eq` type class. And the reason is that you thought the behaviors it provides are useful for many types. For example, the Blockchain type:


```haskell
-- Create data type
data Blockchain = Cardano | Ethereum | Bitcoin

-- Create instance of Eq
instance Eq Blockchain where
    Cardano == Cardano = True
    Ethereum == Ethereum = True
    Bitcoin == Bitcoin = True
    _ == _ = False


-- Test
Cardano /= Cardano
```


    False


Now, the `==` and `/=` are truly overloaded because they have more than one definition depending on the type of values they are applied to.

We did it!! And we're on a roll, so let's keep going!

So far, we've created two instances of the `Eq` type class. Both for non-parameterized types. Let's learn how we can define an instance for a parameterized type.

### Defining an instance for a parameterized type

To create an instance for a parameterized type, first, we need the parameterized type:


```haskell
data Box a = Empty | Has a
```

Now we can create our instance. But we can't do it like this:

```haskell
instance Eq Box where
-- ...
```

Why? Well, if we take a look at the type class using the `:i` command:

```haskell
type Eq :: * -> Constraint -- Eq takes a concrete type and returns a Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}

-- ... and a bunch of instances.
```

We get remided that the type variable `a` is a concrete type. We can see this in two places:
- If we check the types of the functions, we see that the type variable `a` is alone between arrows, so it represents a concrete type by itself.
- And, because of that, the kind of `Eq` (`type Eq :: * -> Constraint`) clearly states that it takes a concrete type and produces a `Constraint`.

Type classes always have a kind that returns a `Constraint` because type classes don't produce a type. They produce a constraint for polymorphic values. So, If we see a kind that ends in `Constraint`, we know it's a type class, and it goes to the left of the `=>` arrow to constraint polymorphic types. 

On top of that, we don't need to check the functions to know how the type class uses the type variable `a`. The kind already tells us if it needs a concrete type or a specific type constructor.

So, because of `Eq :: * -> Constraint`, we know that the `a` in `Eq a` is a concrete type. But if we check the kind of `Box`:


```haskell
:k Box
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
</style><span class='get-type'>Box :: * -> *</span>


We see that it's not a concrete type but a type constructor that takes one type as a parameter and returns a concrete type.

So, what do we do? We could apply `Box` to another type to get a concrete type, like this:


```haskell
:k Box Int
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
</style><span class='get-type'>Box Int :: *</span>


That technically gives us a concrete type, so we could create the instances like this:

```haskell
instance Eq (Box Int) where
-- ...

instance Eq (Box String) where
-- ...

instance Eq (Box PaymentMethod) where
-- ...

--- etc
```

And it would work perfectly. But, Hmm, this is a lot of work. And we already went through this when defining functions and solved it with type variables. This time is no different!:

```haskell
instance Eq (Box a) where
-- ...
```

By defining this instance, all the types created using the `Box` type constructor (like `Box String` or `Box Int`) will be an instance of `Eq`.

Now, wait a second. How do we define the instance if we don't know the type of the value inside the box? Well, if we decide that:

- Two boxes containing equal elements are equal.
- Two empty boxes are equal.
- And everything else is different.

We can define the behaviors like this:


```haskell
instance Eq (Box a) where
  Has x == Has y = x == y
  Empty == Empty = True
  _ == _ = False
```


    <interactive>:2:20: error:
        • No instance for (Eq a) arising from a use of ‘==’
          Possible fix: add (Eq a) to the context of the instance declaration
        • In the expression: x == y
          In an equation for ‘==’: Has x == Has y = x == y
          In the instance declaration for ‘Eq (Box a)’


In the first formula, we define `==` for the `Box a` type by applying `==` to the `a` type it contains. Because `Has x` is of type `Box a`, `x` is of type `a`. Same for the rest of values. So, if both boxes contain the same element, the boxes themselves are the same. Else, they are different. So, were making the instance of `Box a` depend on the instance of `a`. 

In the second formula, we specify that if both boxes are empty, they are equal.

For every other case, the boxes are different.

This makes sense, but there's a HUGE oversight on our part! Did you spot it? It's ok if you didn't. That's what the compiler is here for! If we run the cell, we'll get a compiler error:

```
No instance for (Eq a) arising from a use of ‘==’
```

Ok, so the compiler is telling us that we're applying the `==` function to a type that doesn't have an instance of `Eq`.

Where are we doing that?

```
In the expression: x == y
In an equation for ‘==’: Has x == Has y = x == y
In the instance declaration for ‘Eq (Box a)’
```
The compiler is correct! We're using `==` between two values (`x` and `y`) of type `a` without making sure that the `a` type itself is an instance of `Eq`!

So, what should we do? Well, the compiler also told us how to fix this:

```
Possible fix: add (Eq a) to the context of the instance declaration
```

Same as with functions, we can add the constraint that the type `a` in the instance of `Eq (Box a)` has to also be an instance of the `Eq` type class. Like this:


```haskell
instance (Eq a) => Eq (Box a) where
  Has x == Has y = x == y
  Empty == Empty = True
  _ == _ = False
```

This way, the type `Box a` will be an instance of `Eq` for all the types `a` that are also an instance of `Eq`.

Aaaaaaand we're done! We can use this new instance like this:


```haskell
Has Cardano /= Has Ethereum -- True

Has Card == Empty           -- False

Has Bitcoin /= Has Bitcoin  -- False


data Choice = Yes | No      -- We didn't create an Eq instance for Choice

Has Yes == Has No           -- Angry compiler: There's no instance for (Eq Choice), you fool!
```


    True



    False



    False



    <interactive>:1:1: error:
        • No instance for (Eq Choice) arising from a use of ‘==’
        • In the expression: Has Yes == Has No
          In an equation for ‘it’: it = Has Yes == Has No


So, even when wrapping the type inside another type, the compiler will still protect us of our human mistakes.

Ok. Now that we did everything step-by-step with the `Eq` type class, let's do everything again, but quicker and with a new type class that it's not part of standard Haskell.

## The  `WeAccept` Type Class

Imagine we're writing an app that accepts payments for a company, and this company doesn't accept all payment methods, blockchains, and countries. So, you have to create functions to check that:


```haskell
-- Function to check if we accept that payment method
weAcceptPayment :: PaymentMethod -> Bool
weAcceptPayment p = case p of
   Cash -> False
   Card -> True
   CC   -> True

-- Function to check if we accept that blockchain
weAcceptBlockchain :: Blockchain -> Bool
weAcceptBlockchain b = case b of
   Bitcoin  -> True
   Ethereum -> False
   Cardano  -> True

-- Country type
newtype Country = Country { countryName :: String }

-- Function to check if we accept that country
weAcceptCountry :: Country -> Bool
weAcceptCountry c = case countryName c of
   "Mordor"  -> False
   _         -> True
```

Seeing this code, we realize that this behavior of checking if the company accepts something could be used in many other aspects. Like providers, technologies, etc. There are a lot of things a company could decide to accept or not.

To avoid having a bunch of different functions that do the same all over your code, we decide to create a type class that represents this behavior.

And that type class looks like this:


```haskell
-- Creating WeAccept type class
class WeAccept a where
  weAccept :: a -> Bool

-- Checking kind of WeAccept
:k WeAccept
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
</style><span class='get-type'>WeAccept :: * -> Constraint</span>


Now that we have our type class, we can create the instances for `PaymentMethod`, `Blockchain`, `Country`, and even `Box` like this:


```haskell
instance WeAccept PaymentMethod where
  weAccept x = case x of
   Cash -> False
   Card -> True
   CC   -> True

instance WeAccept Blockchain where
  weAccept x = case x of
   Bitcoin  -> True
   Ethereum -> False
   Cardano  -> True

instance WeAccept Country where
  weAccept x = case countryName x of
    "Mordor" -> False
    _        -> True

instance (WeAccept a) => WeAccept (Box a) where
  weAccept (Has x) = weAccept x
  weAccept Empty   = False
```

And done! This gives us the ability to apply the overloaded `weAccept` function to three different types:


```haskell
weAccept Cardano
weAccept Cash
weAccept (Country "Mordor")
weAccept (Has Bitcoin)
```


    True



    False



    False



    True


We can also create functions that can be applied to all the types that are instances of `WeAccept`:


```haskell
-- Creating fancyFunction
fancyFunction :: (WeAccept a) => a -> String
fancyFunction x =
  if weAccept x
    then "Do something fancy"
    else "Don't do it!"
    
-- Using fancyFunction
fancyFunction Cardano
fancyFunction Card
fancyFunction (Country "Mordor")
fancyFunction (Has Bitcoin)
```


    "Do something fancy"



    "Do something fancy"



    "Don't do it!"



    "Do something fancy"


Another type class under our belt! It's getting easier by the minute! 

We'll do one more example before continuning to the next section. This one is a bit more difficult, but if you understand it, you'll be able to understand any type class! No matter how complicated it gets!

## The `Container` Type Class

This is the scenario: We're working on a logistics software that has two different types of packages. A regular box that may or may not contain something, and a present, that may or may not contain something, but that it always has a name tag of who's the present for. So, we have these two types:


```haskell
data Box a       = Empty          | Has a            deriving (Show)
data Present t a = EmptyPresent t | PresentFor t a   deriving (Show)

:k Box
:k Present
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
</style><span class='get-type'>Box :: * -> *</span>



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
</style><span class='get-type'>Present :: * -> * -> *</span>


Because we decided that the tag of the present (`t`) can be a number, a name, or anything else that could identify a customer, we'll also parameterize its type.

Now, a few parts of the process require functions common to both types. We need:
- One to check if a box or present is empty.
- One to check if a specific value is contained inside the box or present.
- And one to replace the contents of the box or present.

Instead of writing the functions by themselves and then transforming them to a type class and instances as we did in the two previous examples, let's go straight to the type class.

```haskell
class Container c where
    isEmpty  ::  -- ...
    contains ::  -- ...
    replace  ::  -- ...
```

The type class will be called `Container` because it provides behaviors related to containers. The type variable is called `c` because it's a container.

Now, let's write down the type signatures. We'll start with the `replace` function. Cause why not?

```haskell
class Container c where
    isEmpty  ::  -- ...
    contains ::  -- ...
    replace  ::  c a -> b -> c b
```

`replace` takes two inputs:
- A container `c` that has some value of type—let's say `a`—inside.
- And another value that can be of the same or different type than the one inside the container. Let's call it `b`. 

The function replaces the value of type `a` inside the container with the one of type `b`. So, in the end, we get a value of type `c b` because the value it contains is now of type `b`.

Now, let's do the `contains` function:

```haskell
class Container c where
    isEmpty  ::  -- ...
    contains ::  (Eq a) => c a -> a -> Bool
    replace  ::  c a -> b -> c b
```

`contains` takes two inputs:
- A container `c` that has some value of type `a` inside.
- And another value that will be compared to the one inside the container. So it needs to be of the same type `a`, and an instance of `Eq` because we'll need to use `==` to check if it's the same value.

The function takes the value, checks if it's the same as the one inside the container, and returns `True` if it is and `False` if it isn't. So, we return a boolean.

And finally, let's do the `isEmpty` function:


```haskell
class Container c where
    isEmpty  ::  c a -> Bool
    contains ::  (Eq a) => c a -> a -> Bool
    replace  ::  c a -> b -> c b
```

`isEmpty` takes one input:
- A container `c` that has some value of type `a` inside.

The function takes the container and returns `True` if it contains a value and `False` if it doesn't. So it returns a value of type `Bool`.

Our type class is ready to go! 

And because each `->` (arrow) separates a value, and all values need to have a concrete type, we know that both `a` and `b` are concrete types by themselves. Because they are alone between arrows.

Using the same reasoning, we know that `c a` and `c b` have to be concrete types. And because `a` and `b` are concrete types, this means that `c` is a type constructor that takes a concrete type and returns a concrete type.

We can see this if we check the kind of our type class:


```haskell
:k Container
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
</style><span class='get-type'>Container :: (* -> *) -> Constraint</span>


Now that we have our type class, let's create the instances for the `Box` type:


```haskell
-- class Container c where
--     isEmpty  :: c a -> Bool
--     contains :: (Eq a) => c a -> a -> Bool
--     replace  :: c a -> b -> c b

instance Container Box where

    isEmpty Empty = True
    isEmpty _     = False
    
    contains (Has x) y = x == y
    contains Empty   _ = False
 
    replace _ x = Has x
    

:k Box
:k Container
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
</style><span class='get-type'>Box :: * -> *</span>



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
</style><span class='get-type'>Container :: (* -> *) -> Constraint</span>


Notice that we create an instance for `Box`, not `Box a`. For the `Eq` type class, we applied `Box` to the type variable `a` to obtain the concrete type `Box a` because the `Eq` type class needed a concrete type as a parameter. But `Container` takes a constructor of kind `* -> *`, which is the same kind as `Box`. So we have to pass `Box` without applying it to anything.

The actual implementation of the functions is pretty straightforward. Because `Box` has two constructors, we have two formulas per function.

Now let's create the instance for the `Present` type:


```haskell
-- class Container c where
--     isEmpty  :: c a -> Bool
--     contains :: (Eq a) => c a -> a -> Bool
--     replace  :: c a -> b -> c b


instance Container (Present t) where
    
    isEmpty (EmptyPresent _) = True
    isEmpty _                = False
    
    contains (PresentFor _ x) y = x == y
    contains (EmptyPresent _) _ = False
    
    replace (PresentFor tag _) x = PresentFor tag x
    replace (EmptyPresent tag) x = PresentFor tag x


:k Present
:k Container
:k Present String
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
</style><span class='get-type'>Present :: * -> * -> *</span>



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
</style><span class='get-type'>Container :: (* -> *) -> Constraint</span>



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
</style><span class='get-type'>Present String :: * -> *</span>


Now, the instance is for the `Present t` type constructor. This is because `Present` by itself has kind `* -> * -> *`, but because `Container` takes a type constructor of kind `* -> *`, we have to apply `Present` to a type—like `Present String`—to obtain the kind we need. And because we want to be able to use any type as a tag, we use the type variable `t`.

So, this part is important. The `t` in `Present t` is the tag. And the whole `Present t` type constructor is `c`. We can treat the `Present t` type constructor as `c` because it's a type that never changes. We don't change the tag's type in any of the functions. But we do modify the type of the contents in the `replace` function. When we use `replace`, the type of the contents can change from `a` to `b`, so we can't treat them as a constant type like `t`. That's why they are parameters to the `c` type constructor, so we can change the type in the `replace` function if we need to.

Same as before, the actual implementation of the functions are straight forward.

And to rip the rewards of our work, here are a few examples using our new type class behaviors:


```haskell
Has False `contains` False         -- True

isEmpty (Has 'a')                  -- False

PresentFor "Tommy" 5 `contains` 5  -- True

PresentFor "Tommy" 5 `replace` "Arduino"   -- PresentFor "Tommy" "Arduino"


guessWhat'sInside :: (Container a, Eq b) => a b -> b -> String
guessWhat'sInside x y =
  if x `contains` y 
    then "You're right!!"
    else "WROOONG!"

guessWhat'sInside (PresentFor "Mary" "A Raspberry Pi!") "A Ponny!"  -- **Mary's Dissapointment increasses**
guessWhat'sInside (Has 1) 15
```


    True



    False



    True



    PresentFor "Tommy" "Arduino"



    "WROOONG!"



    "WROOONG!"


Understanding this type class and instances was the trickiest part of the lesson. It might take a while to fully grasp what we just saw. But don't worry, if something doesn't click, it will with some practice. That's why it's important to do the homework.

Now, let's learn about subclassing. After everything we went through, this is a piece of cake.

## Exploring the `Ord` type class (Subclassing)

We never talked about subclassing before, but you already know how it works.

Let's see it in practice while defining an instance for the `Ord` type class.

If we run the info command on the `Ord` type class (`:i Ord`), we would get something like this:

```haskell
type Ord :: * -> Constraint         -- Takes a concreate type
class Eq a => Ord a where           -- That "Eq a =>" is new!! 🤔
  compare :: a -> a -> Ordering 
  (<) :: a -> a -> Bool             -- A bunch of functions
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}    -- We can only implement "compare" or "<=".
```

Everything checks out. Except for that `Eq a =>`. We've seen this in both functions and instances. But never on type class definitions.

This (`Eq a =>`) means what you'd imagine:

**To make a type `a` an instance of `Ord`, first we have to make it an instance of `Eq`! Meaning that `Eq` is a prerequisite for `Ord`. In other words, `Eq` is a superclass of `Ord` or `Ord` is a subclass of `Eq`.**

Superclasses allow simpler signatures to be inferred. By saying that a type is an instance of `Ord`, not only do we know that it has the behaviors of `Ord`, but also the behaviors of `Eq`. Also, this allows us to use behaviors of the `Eq` type class to define the instances of the `Ord` type class. Which is actually something that happens in this case. The `Ord` type class uses functions provided by the `Eq` type class.

We can't see it because the info command doesn't show the whole type class definition. Same as when we run the info command for the `Eq` type class, it doesn't show the mutually recursive definitions of `==` and `/=` that we just wrote.

Still, even though we can't see them, we know there are a bunch of function definitions defined in terms of each other. That's why we can implement the entire instance by only defining `compare` or `<=`.

The info command doesn't show all that code because we, the developers, don't care about it. We only want to know:
- Which behaviors come with the type class. To see if it's what we need.
- The kind of the type class and minimum behaviors we need to implement. To only implement those. 
- If it depends on another type class. To implement that one before this one.
- And, finally, which types are already an instance of this type class. To see which types can already use those behaviors.

And that's what the info command shows us.

So, to make a type an instance of `Ord`, first, we have to make it an instance of `Eq`. Luckily, we already created a few instances for `Eq` before, so we're already halfway through if we want to create `Ord` instances for any of those types.

For example, if we want to create an instance of `Box a` for the `Ord` type class, we have to implement one of the functions needed for the minimal complete definition! In this case, we chose the `compare` function:


```haskell
-- type Ord :: * -> Constraint  
-- class Eq a => Ord a where   
--   compare :: a -> a -> Ordering 

instance (Ord a) => Ord (Box a) where
  Has x `compare` Has y = x `compare` y
  Empty `compare` Has _ = LT
  Has _ `compare` Empty = GT
  Empty `compare` Empty = EQ
  

Has 9 >= Has 5
Empty `compare` Has 0
Empty < Empty
```


    True



    LT



    False


This is what is happening here:

- If both boxes have some value inside, we compare the values. And because we're applying the `compare` function to `x` and `y` of type `a`, we need to add the constraint that the `a` type has to be an instance of `Ord`.
- If one of the boxes is `Empty` and the other isn't, it doesn't matter what's inside the one that has something. It will always be greater than the `Empty` one. Because I said so.
- If both are `Empty`, of course, they are equal.

And Boom! that's it!

We created:
- The `Eq` type class with 3 different instances.
- The `WeAccept` type class with 4 instances.
- Then, the `Container` type class with 3 instances.
- And finally, we made a type an instance of the `Ord` type class.

**Congratulations! 🎉 You know everything needed to work with type classes!!**

As the final section of this lesson, we'll learn how and when to automatically derive instances. Saving us some precious time and reducing the amount of code we have to maintain.

## Deriving

Derived instances are an automatic way of making a type an instance a type class. This is possible because many common type classes are usually implemented the same way. And some clever guys with PhDs figured out how to generate this code based on the type's definition.

This is limited to `Eq`, `Ord`, `Enum`, `Show`, and others defined in either the Prelude or a standard library—libraries that come with Haskell and that we'll explore in future lessons. For now, think that all the type classes we used until now, and that we didn't create ourselves, can be derived.

To use this feature, add the `deriving` keyword at the end of your type declaration with the names of all the type classes you want to derive. For example, if we do this:


```haskell
data Choice = No | Idk | Yes deriving (Eq, Ord, Show, Bounded, Enum)
```


```haskell
Yes /= No             -- Are these values different?   (Behavior from Eq)

Yes > No              -- Is Yes bigger than No?        (Behavior from Ord)

show Yes              -- Transform Yes to String       (Behavior from Show)

(minBound) :: Choice  -- Smallest value of type Choice (Behavior from Bounded)

succ No               -- Successor of No               (Behavior from Enum)
```


    True



    True



    "Yes"



    No



    Idk


And that's it!! Your `Choice` type has the behaviors provided by all those type classes.  

So, if we could do that from the start, why in the world would you care for manually deriving instances?

Well... One reason is that not all type classes can be derived. And another is that deriving can sometimes go wrong.

### Deriving can go wrong

Each type class has its own set of rules for deriving instances. For example, when deriving the `Ord` type, value constructors defined earlier are smaller. So, in this case:


```haskell
data PaymentMethod = Cash | Card | CC deriving (Eq, Ord)

Cash > Card
Card < CC
CC `compare` Cash
```


    False



    True



    GT


`Cash` is smaller than `Card`, which is smaller than `CC`.

And in this case:


```haskell
data Box a = Empty | Has a deriving (Eq, Ord)

Has 5 `compare` Has 6
Has "Hi" >= Has "Hello!"
```


    LT



    True


If a value constructor has a parameter (`Has a`), and two values are made from the same constructor (`Has 5` and `Has 6`), the parameters are compared (like we did when we defined the instances ourselves).

Those are the rules the compiler follows to automatically create the `Ord` instance for your type. Other type classes have other rules. We won't go over the rules of each type class, but I'll provide a [link](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html) with a short explanation in the interactive lesson. In case you want to learn more.

Now, let's say we want to use a type to manage lengths for Civil engineering software.

We work with both meters and kilometers, but because we don't want to accidentally mix those and get a potentially catastrophic error, we define a data type with two constructors. One for meters and one for kilometers. Both contain a value of type `Double`. We'll also derive the `Eq` type class.


```haskell
data Length = M Double | Km Double deriving (Eq)
```

But, as soon as we start using this data type, we find a little problem. We know that 1000 Meters equals 1 Kilometer, but when we test this in our code, we get that it's not!:


```haskell
M 1000 == Km 1 -- False
```


    False


That's because when we derived `Eq`, Haskell generated this code:

```haskell
instance Eq Length where
  (==) (M  x) (M  y) = x == y 
  (==) (Km x) (Km y) = x == y 
  (==) _      _      = False
```

This works great if we're comparing meters to meters and kilometers to kilometers. But we have the wrong implementation to compare between constructors because Haskell doesn't know that the values of different constructors relate in any way!! Haskell just assumed that if the constructors are different, the values are too!

So, in this case, we have to write the instance ourselves to take into account the relationship between the constructors. Like this:


```haskell
data Length = M Double | Km Double

instance Eq Length where
  (==) (M  x) (M  y) = x == y
  (==) (Km x) (Km y) = x == y
  (==) (M  x) (Km y) = x == 1000 * y
  (==) (Km x) (M  y) = x * 1000 == y


M 3000 == Km 3   -- True
Km 7   /= M 14   -- True
```


    True



    True


That's why it's a good idea to be conscious of how each type class is derived. To know when you can derive them and when you have to write the instance by hand.

And to finish the lesson, here are a few tips for real-world coding:

### Tips for real-world coding

- Everything I explained here today applies to all type classes.

- We don't define type classes that often. Usually, the ones that come with Haskell are all we need.

- We do implement instances quite a lot. And it's usually (but not always) a good idea to derive them. If you're in doubt, try automatic deriving and check your assumptions. You can always come back and manually define the instances.

- You can peek at a type class definition using `:i` on GHCi to see the minimum behaviors to implement when creating your instance. Implement those, and you're done.

# That's it for today!
