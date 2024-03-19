# Abstractions, Semigroup, and Monoid

This is the first lesson of the "Abstracting Patterns" section of the course. In this lesson, we'll cover:

## Outline

- What does it mean to abstract a pattern?
- Why abstracting patterns (in general)?
- Teaser: Why abstracting `Semigroup` and `Monoid`?
- The `Semigroup` type class
- The `Monoid` type class
- What can we do with `Semigroup` and `Monoid`?

## What does it mean to abstract a pattern?

We humans are very good at detecting patterns. For example, in the 6th lesson of this course, we wrote these functions:

```haskell
sum'     :: [Int]  -> Int
sum'     []     = 0
sum'     (x:xs) = x + sum' xs

product' :: [Int]  -> Int
product' []     = 1
product' (x:xs) = x * product' xs

and'     :: [Bool] -> Bool
and'     []     = True
and'     (x:xs) = x && and' xs
```

We figured out that there was a repeating pattern in all of those functions, so we created a single one that contains that pattern and takes whatever is different as arguments:

```haskell
--           +/*/&&    -> 0/1/True -> [Int]/[Bool] -> Int/Bool
foldr :: (a -> b -> b) ->    b     ->     [a]      ->    b
foldr _ v [] =  v
foldr f v (x:xs) = f x (foldr f v xs)
```

And then, because we had a function that represented the abstract idea of:

"Applying a function to combine the first value of a list and the result of recursively applying the same function to the rest of the list"

Or more sucinctly:

"Reducing a list using a binary operator"

We replaced the implementation of the original functions with the abstraction like this:

```haskell
sum'     :: [Int]  -> Int
sum'     = foldr (+) 0 -- We partially apply foldr

product' :: [Int]  -> Int
product' = foldr (*) 1

and'     :: [Bool] -> Bool
and'     = foldr (&&) True
```

That seires of steps:

1. Write some code.
2. Identifying a pattern.
3. Create a structure* to contain that pattern (if useful).
4. Use that structure instead of explicitly writting the pattern.

<sub>*By "structure," we mean types, functions, and type classes. Other programming languages may use different ones (like OOP classes).</sub>

Ok, so this is what we mean when we talk about abstracting a pattern.

As a small caveat, it's important to note that we shouldn't extract all patterns. As I said before, we humans are very good at detecting patterns, but not all of them are worthy of abstraction. But don't worry about that for now. You'll learn to tell the difference with practice.

## Why abstracting patterns (in general)?

One somewhat reasonable question would be to ask: "Why should I abstract patterns?" And there are several reasons, the most important ones are:

- To easily reuse code

As we showed in the previous `foldr` example, now you have to implement the recursive pattern once, and you can use it anywhere.

- To hide the unimportant details

Also, in the previous example, we hid the details of how exactly we implement the recursion inside `foldr`. Thanks to that, the code becomes a short one-liner that only shows what we care about: Which binary function we apply and what is the starting value.

- To have clear and concise code that you (and others) can quickly understand

By using `foldr` instead of explicitly writing the pattern, the reader instantly understands what we're doing. We're folding a list. That's it. 1 second is enough to know what this line does and keep moving. 

These reasons apply for every correctly derived abstraction. But what about `Semigroup` and `Monoid` specifically?

## Teaser: Why abstracting `Semigroup` and `Monoid`?

To make sure I get your full attention throughout the rest of this lesson, I'll share a real-world problem that becomes significantly easier by abstracting `Semigroup` and `Monoid`, and that is:

### Scalability

More specifically:

- **Scaling computations**
- **Scaling result complexity without increasing code complexity**

If you've worked with any programming language in production, you'll likely know this is a tough and complex problem. So, we developers need all the help we can get.

At the end of the lesson, after learning about `Semigroup` and `Monoid`, we'll see how these abstractions allow us to more easily scale.

## The `Semigroup` type class

In the first example, we abstracted away a pattern into a function, and now we'll abstract away into a type class. This is not something new. If you think about it:

``` haskell
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

The `Num` type class is an abstraction of the properties and behaviors a number should have. All numbers should be able to be added, subtracted, multiplied, and so on. Some types we think of as numbers behave fundamentally differently in some ways. For example, we can get fractional numbers with `Float`, but we can not with `Integer`. So, the `Num` type class abstracts away only the behaviors every number-like type should have.

We are going to do the same someone else did for numbers, but for a different concept. Take a look at this code:

```haskell
("Abstract " ++ "me") ++ "!"  -- "Abstract me!"
"Abstract " ++ ("me" ++ "!")  -- "Abstract me!"

(2 * 3) * 4  -- 24
2 * (3 * 4)  -- 24

(True && False) && True  -- False
True && (False && True)  -- False
```

What do we have in common in all three cases? Well, in all three cases, there's a binary function (a function that takes two parameters) that somehow combines two values of one type to produce a new value of the same type:

```haskell
(++) :: [a] -> [a] -> [a]

(*) :: Num a => a -> a -> a
 
(&&) :: Bool -> Bool -> Bool
```

And, on top of that, the binary operation is associative! Meaning the order in which we apply the binary function doesn't matter. And that's it! That's the whole concept:

 A `Semigroup` is a type that has an associative binary operation.

It seems like an arbitrary choice. Like, why this abstraction instead of another? Well, it'll be clear why at the end of the lesson. However, as with all the abstractions we'll cover in this course, it boils down to this: We realized they are more useful than others.

Ok. So, now that we have the concept we want to represent and know that we need it to be available for multiple types, we'll create a type class that represents it. 

Aaand... this is it:

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

I know, I know, a bit anti-climactic. All that hype for a two-line type class.

We chose to use an operator that looks like a diamond instead of a regular prefix function because the most common use case (as we saw in the examples we used to extract the pattern) is to apply the binary function as an infix function.

Now, wait a minute... what about the assosociativity? Where does that appear in the code? 

Well, the sad truth is that not even Haskell's type system, the most powerful of all general-purpose mainstream languages, can restrict this property. And, because we can not use code to transmit these requirements, we use laws written in plain text and kindly ask developers to follow them. Of course, developers follow them because these laws bring a lot of value.

In this case, every time you create an instance of `Semigroup`, you have to make sure to satisfy the associativity law:

**Associativity law:**
```haskell
x <> (y <> z) = (x <> y) <> z
```

Ok, we have our abstraction ready. Let's implement an instance to see how that would look, starting with the list instance. 

We have to choose a binary operation. This is really easy for lists. If you explore the `Prelude` and the `Data.List` modules, or even easier, if you look up the type using Hoogle, you'll find out that there's only one operator that takes two lists to generate another list, and on top of that, it's associative! And that's the `++` operator that appends two lists:

```haskell
("Abstract " ++ "me") ++ "!"  -- "Abstract me!"
"Abstract " ++ ("me" ++ "!")  -- "Abstract me!"
```

Awesome! We have our operator. And now, we can implement our instance. Just this time, I'll show all possible ways to do it. But, I trust that, by now, you could figure this out by yourself:

```haskell
instance Semigroup [a] where
  (<>) = (++)

-- same as doing:

instance Semigroup [a] where
  (<>) []     ys = ys
  (<>) (x:xs) ys = x : xs <> ys
  
-- same as doing:

instance Semigroup [a] where
  []     <> ys = ys
  (x:xs) <> ys = x : xs <> ys
  
```

All three implementations are the same thing written differently, so you could choose whichever you want. In this case, because the operator is already defined, the best would be to just use it, as shown in the first implementation. And, if you're curious, that's how it's actually defined in the `base` library.

To do a rough check that the associativity law holds, we could do a few tests by hand:


```haskell
("is " <> "this ") <> "True?" == "is " <> ("this " <> "True?")

(([1] <> [2]) <> []) <> [3,4] == [1] <> ([2] <> ([] <> [3,4]))

([True] <> ([True] <> [False])) == [True] <> [True] <> [False]
```


    True



    True



    True


Of course, this is not proof that the law holds. It's just a suggestion that it seems to work, which is more than enough for us. However, thanks to Haskell's purity, we could prove this law by induction or property testing. That's out of the scope of this course, but I'll link an explanation in the video description. Just in case you're curious.

Ok, done! We have our first instance of `Semigroup`. It seems we're done with Semigroup, but there's one more thing to take into account: What if there's no clear answer as to which operation we should use? For example, if we're using numbers, a quick search would give us four binary operations:

```haskell
(+)       :: Num a => a -> a -> a
(*)       :: Num a => a -> a -> a
(-)       :: Num a => a -> a -> a
substract :: Num a => a -> a -> a
```

But wait, if we quickly check of associativity:


```haskell
(2 + 3) + 4 -- 9
2 + (3 + 4) -- 9 ✅

(2 * 3) * 4 -- 24
2 * (3 * 4) -- 24 ✅

(2 - 3) - 4 -- -5
2 - (3 - 4) -- 3 ❌

(2 `subtract` 3) `subtract` 4 -- 3
2 `subtract` (3 `subtract` 4) -- -1 ❌
```


    9



    9



    24



    24



    -5



    3



    3



    -1


We see that the `(-)` (minus) and `subtract` functions aren't associative. This makes sense because subtraction isn't associative in maths, either.

So, we're left with just two functions:

```haskell
(+) :: Num a => a -> a -> a

(*) :: Num a => a -> a -> a
```

Which one should we use? Both functions satisfy the `Semigroup` requirements of being an associative binary function. But we can not choose more than one... or can we? 

It turns out that some types, in this case, all numeric types, have more than one valid `Semigroup` instance. To resolve this, we create one `newtype` per valid and useful operation that wraps the original type. That way, we can implement as many instances as we need because they are different types!

In the current case, because both the sum and product operations are valuable, we'll wrap a type in the `Sum` and `Product` `newtypes`:


```haskell
newtype Sum a = Sum { getSum :: a }
  deriving (Show, Eq)

newtype Product a = Product { getProduct :: a }
  deriving (Show, Eq)
```

As you can see, we just wrapped the `a` type with a `Sum` and `Product` constructors to get the new `Sum` and `Product` types. On top of that, we used record syntax to easily extract the wrapped value without the need for pattern-matching in case we want to.

And now comes the magic. We'll implement their `Semigroup` instances using their corresponding binary operation:


```haskell
instance Num a => Semigroup (Sum a) where
  (Sum a) <> (Sum b) = Sum (a + b)

instance Num a => Semigroup (Product a) where
  (Product a) <> (Product b) = Product (a * b)
```

As you can see, the only thing that changes is that we have to make sure the type inside `Sum` and `Product` are also instances of `Num` in order to have the `+` and `*` operators available to us.

Other than that, it's just pattern-matching to unwrap the numbers from the parameters, applying the binary operation to the numbers, and wrapping the result.

Let's try them!:


```haskell
Sum 3 <> Sum 2

Product 5 <> Product 9

(Sum 4 <> Sum 5) <> Sum 1 == Sum 4 <> (Sum 5 <> Sum 1)

getProduct $ Product 3 <> Product 5 <> Product 2

-- Sum 9 <> Product 10 -- ❌ Won't compile! Different types!!
```


    Sum {getSum = 5}



    Product {getProduct = 45}



    True



    30


It works! This is not the only case. We also have two options between all the orderable types:

```haskell
max :: Ord a => a -> a -> a

min :: Ord a => a -> a -> a
```

Both `max` and `min` functions are associative binary operations, and both make sense to use, so we do the same. We create `newtype` wrappers:


```haskell
newtype Max a = Max { getMax :: a }
  deriving (Show, Eq)

newtype Min a = Min { getMin :: a }
  deriving (Show, Eq)
```

And then, we create the `Semigroup` instances with the corresponding associative binary operations:


```haskell
instance Ord a => Semigroup (Max a) where
  (Max a) <> (Max b) = Max (a `max` b)

instance Ord a => Semigroup (Min a) where
  (Min a) <> (Min b) = Min (a `min` b)
```

Finally, we test it:


```haskell
Min 3 <> Min 6

Max 9 <> Max 0

(Min 4 <> Min 5) <> Min 1 == Min 4 <> (Min 5 <> Min 1)

getMax $ Max 3 <> Max 5 <> Max 2
```


    Min {getMin = 3}



    Max {getMax = 9}



    True



    5


The case for booleans is very similar; So similar that, in fact, you'll have to implement it as part of this lesson's homework.

But before we move on, let's implement a `Semigroup` for a type we came up with ourselves. For example, the `Severity` type. A type that represents the severity of an emergency:


```haskell
data Severity = Low | Medium | High | Critical deriving (Show, Eq)
```

We don't have any pre-existent associative binary operations, so we'll have to come up with one. What do you think would be a good associative binary operation for severity?: 

```haskell
(<>) :: Severity -> Severity -> Severity
```

Pause the video if you want to think about it for a bit. Better yet, try to implement it yourself!

Ok. So, we want to combine severity levels. It makes sense that if we have two emergencies of the same severity, we should return one with the same severity. And if they are of different severities, we should return the highest one. So, we could define `Severity`'s `Semigroup` instance like this: 


```haskell
instance Semigroup Severity where
  Critical <> _ = Critical
  _ <> Critical = Critical
  High <> _     = High
  _ <> High     = High
  Medium <> _   = Medium
  _ <> Medium   = Medium
  _ <> _        = Low
```

I think this makes quite a lot of sense. Let's check if the binary operation is actually associative: 


```haskell
High <> Medium

Low <> Medium <> Low

(High <> Low) <> Critical == High <> (Low <> Critical)
```


    High



    Medium



    True


And that's it! We created our fifth `Semigroup` instance. If you understand everything up until now, the next abstraction will be a piece of cake. So, let's talk about the `Monoid` type class:

## The `Monoid` type class

The `Monoid` type class builds on top of the `Semigroup` type class to add a small but significant extra behavior. Let's take a look at the same example we saw at the beginning, but with a slight tweak: 

```haskell
("Abstract " ++ "me") ++ "!"        -- "Abstract me!"
"Abstract " ++ "" ++ ("me" ++ "!")  -- "Abstract me!"

(2 * 3) * 4      -- 24
2 * 1 * (3 * 4)  -- 24

(True && False) && True          -- False
True && True && (False && True)  -- False
```

Do you notice the changes I made in the code? And what about the changes in the result?

As you can see, I added one more operation in the second line of each example, but it doesn't affect the end result because one of the values doesn't do anything. We call a value that doesn't modify the result: The "Identity" value. It's not the first time we encountered this concept. We first learned about identities when we learned about recursion and how vital identity values are in defining the base cases.

And as you can see, the `1` is the identity for multiplication, the `True` is the identity for `&&`, and the empty string is the identity for concatenating `String`s, which, more generally speaking, means that the empty list is the identity of concatenating lists.

So, if we speel it out, the pattern we're seeing right here is:

A `Monoid` is a type that has an associative binary operation with an identity.

But we already have a type class representing an associative binary operation. So, instead of repeating ourselves, we can make `Monoid` a subclass of `Semigroup` and add only the identity. Something like this:

```haskell
class Semigroup a => Monoid a where
  mempty :: a
```

Here, the `mempty` value represents the identity. It's called like that due to convention. You can read it as `m` (from `Monoid`) `empty`. 

And this would be conceptually it. But, if we take a look at the actual `Monoid` type class in Haskell, it might look like this:

```haskell
class Semigroup a => Monoid a where
  mempty  :: a             -- Identity element
  mappend :: a -> a -> a   -- <>
  mconcat :: [a] -> a      -- foldr <> mempty
  {-# MINIMAL mempty | mconcat #-}
```

And why is that? Why the extra functions?

Well, because in previous versions of Haskell, we didn't have the `Semigroup` type class. The `Monoid` type class was self-contained, and needed to define its own associative binary operator. The "monoid append" or `mappend` function was the associative binary operation we defined in `Semigroup`, and the "monoid concat" or `mconcat` function is a behavior that we get for free thanks to having the `mempty` and `mappend` functions. It's just `foldr` applied to the binary operator and `mempty`.

I say that it "might look like this" because, by the time you're watching this video, we might not have `mappend` in `Monoid` anymore since it's redundant now that we have `Semigroup`. 

We didn't remove `mappend` from `Monoid` when `Semigroup` was introduced because that would've broken virtually every program written in Haskell. So, to avoid receiving angry emails from every Haskell developer, the maintainers phased out the changes to give everyone the time to catch up before removing it.

Notice, however, that here it's happening the same as it happened with the associativity for `Semigroup`. The restriction that the `mempty` element has to be the identity of the operation is nowhere to be seen. We cannot enforce it with code, so we create laws that indicate to the developer that they have to adhere to some extra rules when implementing `Monoid` instances:

**Right identity**
```haskell
    x <> mempty = x -- e.g.: Sum 4 <> Sum 0 == Sum 4
```
**Left identity**
```haskell
    mempty <> x = x -- e.g.: Sum 0 <> Sum 4 == Sum 4 
```
**Associativity**
```haskell
    x <> (y <> z) = (x <> y) <> z -- (Semigroup law)
```
**Concatenation**
```haskell
    mconcat = foldr (<>) mempty
```

Ok! Let's implement a few `Monoid` Instances.

This is actually pretty easy because we did the hard part when implementing the `Semigroup` type class. These are the `Monoid` instances of all the types we worked with today:


```haskell
--instance Monoid [a] where
--  mempty = []

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Num a => Monoid (Product a) where
  mempty = Product 1

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound

instance Monoid Severity where
  mempty = Low
```

As you can see, all of the instances are pretty straightforward. You have to think of the value that doesn't change the result when applied to the `Semigroup`'s associative binary operation.

- If you sum `0` to a value, you get the same initial value.
- If you multiply a value by `1`, you get the same initial value.
- If you compare if a value is greater than the smallest possible value, you get the same initial value.
- If you compare if a value is smaller than the largest possible value, you get the same initial value.
- If you combine any severity with the lowest one, you get the same initial severity.

Here are a few examples:


```haskell
Sum 2 <> mempty <> Sum 3 == Sum 2 <> Sum 3 -- True

mconcat [Product 2, Product 3, mempty] == Product 2 <> Product 3 -- True

(mempty) :: Max Int -- Max {getMax = -9223372036854775808}

Max 2 <> mempty <> Max 3 :: Max Int -- Max {getMax = 3}

(mempty) :: Min Int -- Min {getMin = 9223372036854775807}

mempty <> Min 2 <> mempty :: Min Int -- Min {getMin = 2}

mconcat [mempty, Medium, mempty, mempty] -- Medium

Sum 9 <> Sum 11 == Sum 9 `mappend` Sum 11 -- True
```


    True



    True



    Max {getMax = -9223372036854775808}



    Max {getMax = 3}



    Min {getMin = 9223372036854775807}



    Min {getMin = 2}



    Medium



    True


And that's it! We created our first 5 instances of `Monoid`.

Now that we know about `Semigroup` and `Monoid`, let's answer the big question. Why are they useful?

## What can I do with `Semigroup` and `Monoid`?

We already established that by abstracting patterns, you can more easily reduce code, hide implementation details that aren't part of the core logic, and have clear and concise code that you (and others) can quickly understand. But that's for all abstractions in general. What do I gain with `Semigroup` and `Monoid` specifically?

### Distributed computation

Imagine you have a dataset of stars with various data points: Their size, mass, brightness, etc. And we want to know which one is the oldest.

We cannot measure the age of a star, so we have to calculate it with a formula that takes all the data of a star and returns its approximate age.

If that computation takes 1 second, and we have a dataset of 1000 stars, it would take around 17 minutes to complete. Not a big deal.

But... that's not a realistic number of stars. Gaia, one of the European Space Agency's telescopes, is currently taking precise measurements of close to 1 billion of the brightest stars in the sky. That's too big of a number to wrap our heads around, so let's say we get our hands on a dataset of 1 million stars. If you want to run your function on that dataset, it will take 114 years to complete. You'll likely be dead before that finishes.

If only there was a way to reduce the wait time...

<img src="../images/distributed_computation.png"
  style="
  display:block;
  margin-left: 20%;
  margin-right: auto;
  width: 64%;"/>

Well, if the result of an `IO` computation is a `Monoid`, the `IO` computation is a `Monoid` too!! This means you could wrap the computation's result with the `Max` monoid, split the work into 200 servers that run in parallel, and merge the results as soon as two consecutive servers finish their computation.

The end result? Instead of waiting 114 years, you have to wait only 6 months. 0.5% of the time it would take using a single server. And, of course, you could keep reducing the wait by spinning more servers.

Now, this feat could be accomplished without a `Semigroup` or `Monoid` instance. But having them made it waaaaay easier. So much easier, in fact, that we didn't have to change the computation!! We just wrapped the result with the `Max` constructor and called it a day. We changed 1 line of code to make our computation parallelizable.

## Scaling result complexity without increasing code complexity

Let's say we have a social media app with a form that a user has to complete with their personal information to create their account:

<img src="../images/monoid_form_1.png"
  style="
  display:block;
  margin-left: 20%;
  margin-right: auto;
  width: 64%;"/>

The user asks to be able to configure their experience, so we add a settings page tha its just a another form:

<img src="../images/monoid_form_2.png"
  style="
  display:block;
  margin-left: 20%;
  margin-right: auto;
  width: 64%;"/>

After some time, we add a form to change their profile image. We need to add this to the settings. But also inside the one to create an account. So we created a reusable component and put it inside both:

<img src="../images/monoid_form_3.png"
  style="
  display:block;
  margin-left: 20%;
  margin-right: auto;
  width: 64%;"/>

Companies also want to use our app, so we added a form for company settings that also has to be inside the one to open the account:

<img src="../images/monoid_form_4.png"
  style="
  display:block;
  margin-left: 20%;
  margin-right: auto;
  width: 64%;"/>

Of course, people lose their passwords, so we'll create a reusable form to change our password that we'll put inside the regular user and company settings: 

<img src="../images/monoid_form_5.png"
  style="
  display:block;
  margin-left: 20%;
  margin-right: auto;
  width: 64%;"/>

Ok, I think that's enough, you get the idea. This is not only about forms. It's the conventional architecture most programs follow:

Combine several components of type `A` to generate a "network" or "structure" of a different type `B`.

That means that every time we add something that generates a more complex end-user experience, the complexity of our code exponentially increases because we have to not only create the new component but also integrate it into the system. And with each addition, it gets harder and harder.

Now, what if the forms themselves where `Semigroup`s? In that case, we don't need to worry about integrating them since that's done by our associative binary operation:

<img src="../images/monoid_form_6.png"
  style="
  display:block;
  margin-left: 15%;
  margin-right: auto;
  width: 68%;"/>

We combine several components of type `A` to generate a new one of the same type `A`.

So, if I want to add a new form now, it doesn't matter if we already have 1, 10, or 100 forms. The complexity is always the same. You still have to build the new form, but you get the integration for free.

Those are two fairly obvious ways that `Semigroup` and `Monoid` instances help. If you want more examples, you'll have to do the homework.

And if you're thinking: "Why did we need to separate `Semigroup` and `Monoid` again? Can't we just have `Monoid` and that's it?"

`Monoid` is more powerful than `Semigroup` because the identity element allows for an easy way to fold and concatenate the elements. So, based on what we know now, it would make sense to only have the `Monoid` type class. But here's the thing: Some types can be instances of `Semigroup` but not of `Monoid`, and it doesn't make sense we have to lose most of the power just because they don't have an identity element. 

For example, there's a type in Haskell that represents a list that can never be empty. And because of that, it doesn't have an identity element and can never be a `Monoid`! Curious about how that works? Well...

# That's it for today!

Do your homework to find out, and I'll see you in the next one!
