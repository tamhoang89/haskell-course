# Intro to Type Classes

## Outline
* The awesomeness of type classes
* What are type classes 
* Common type classes 
	* `Eq`, `Ord`
    * `Num`, `Integral`, `Floating`
	* `Read`, `Show`
* The most general valid type
* Multiple constraints

<div class="alert alert-block alert-info">
<p>
This is an introduction to the concept and use of type classes from the consumer's point of view. Meaning that, as we are developing in Haskell, there are type classes, and we want to understand and use them.
</p>
<p>
Two lessons from now, after learning about how to create types, we'll look at it from the point of view of the type class and instance creator.
</p>
</div>

## The awesomeness of type classes

So far, we learned that, when defining a function, we choose that it could be used either with a specific type like this:

```haskell
sqr :: Int -> Int
sqr v = v * v
```

Which provides a lot of safety because by making sure that your function only takes `Int` types, no matter the value it takes, you can do math with it. But the downside is that you're constrained to use that function with only that type. If you need it for `Double` or `Float,` you have to define it again with a different name like `sqrDouble` and `sqrFloat`.

Or with a polymorphic type like this:

```Haskell
fst :: (a, b) -> a
fst (x, _) = x
```

Which provides a lot of flexibility because you can use values of any type as input, but you lose all the safety that comes with using types. 

So, we have something like this:

```
                    ↑
                    | X (Polym.)    
                    |
      Flexibility   |
                    |
                    |
                    |                X (Types)     
                    |
                    ------------------⟶
                             Safety
```

Type classes are what you get when you're a stubborn programming language developer and want the flexibility of having polymorphic types and the safety of using types all at the same time.

```
                    ↑
                    | X (Polym.)     X (Type Classes)
                    |
      Flexibility   |
                    |
                    |
                    |                X (Types)     
                    |
                    ------------------⟶
                             Safety
```

At the end of the day, what Type Classes do is allow you to use **restricted polymorophic values**. Values can be of different types, but not all of them. Just an authorized subset. This is called *Ad-hoc polymorphism* or *overloading*, but you don't need to remember that right now.

Now that we know why type classes are awesome, let's see what they actually are!

## What are type classes?

If you meet people that belong to the advanced-drawing club, you know they can draw. Why? Because it's one of the requirements to enter the club!

Type classes are like clubs that types can belong to if they have particular behaviors. (Behavior, in this context, means function.) So, a type class specifies a bunch of functions, and each type that belongs to the type class has its own definitions of those functions.

So, from the point of view of the developer that consumes pre-existing types and type classes:

**If you see that a type is an Instance of a type class, you know that it implements and supports the behaviors (functions) of that type class.**

For example, the `Bool` type. To see the type classes to which the `Bool` type belongs, you can use the `:i` (info) command in ghci. If we run `:i Bool`, we get:

```haskell
type Bool :: *
data Bool = False | True
  	-- Defined in ‘GHC.Types’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Show Bool -- Defined in ‘GHC.Show’
instance Read Bool -- Defined in ‘GHC.Read’
instance Bounded Bool -- Defined in ‘GHC.Enum’
```

We'll learn about `type` and `data` in the next lesson. So, if we ignore the first two lines of code, we see a bunch of lines that say `instance ...`. Those lines inform us that the `Bool` type is an instance of the `Eq` type class, the `Ord` type class, etc.

So, `Bool` implements the functions of all those type classes. And, naturally, now we want to know what the behaviors those type classes define. So let's find out!

## Common type classes

Now, we're going to go through the most common type classes. And I'm going to tell you what they represent and their main behaviors. But you don't have to memorize anything about this. After the "creating type classes" lesson, you'll be able to swiftly check everything I'll say in this class. Also, don't worry about the details just yet. We'll spend this and two more lessons on the type system. Use this lesson to start developing the idea of a type class and to get familiar with the most common built-in ones (which are usually the only ones you need).

### The `Eq` type class

The `Eq` type class is all about equality. The types that are instances of the `Eq` type class can say if two values of its type are equal or not by using the `==` (equal) and `/=` (not equal) functions.

And because the `Bool` type is an instance of `Eq`, we know that we can use those two functions to compare values of that type:


```haskell
True == False  -- False

True /= False  -- True
```

And if we check the signatures of the `==` and `/=` functions, we'll see a few new things:

```haskell
(==) :: Eq a => a -> a -> Bool

(/=) :: Eq a => a -> a -> Bool
```

The `=>` symbol is the **class constraint** symbol. It indicates that **a polymorphic type is constrained to be an instance of a type class**.

The code to the right of the fat arrow (`=>`) is the same type signature we've used so far. And the code to the left of the fat arrow (`=>`) indicates the class constraints.

In this case, the code to the right of the fat arrow (`a -> a -> Bool`) indicates that these functions take two polymorphic values and return a `Bool`. Same as always. And the code to the left of the fat arrow (`Eq a`) indicates that the type `a` that is used twice at the right of the fat arrow has to be an instance of the type class `Eq`. 

So, we're constraining (limiting) the types you can pass to these two functions, from all the types to only those that are instances of the `Eq` type class.

And it doesn't stop there. For example, imagine you create this function:


```haskell
func x y = if x == y then x else y
```

You don't do math or manipulate strings. **But you do check if the values are equal. So you want to make sure that this function only accepts values that can be checked for equality**. That's what the `Eq` type class constraint is for. To block you from using types with values that can't be compared.

And because `==` has the `Eq a` constraint and `func` uses `==` inside, Haskell is smart enough to infer that our function's type signature also has that constraint:


```haskell
func :: Eq a => a -> a -> a
func x y = if x == y then x else y
```

And now comes the moment of truth. To how many types can I apply these functions? We know that we can apply it to `Bool` because `Bool` is an instance of `Eq`. But what else? Which are the other instances?

Well.. if you use the `:i Eq` command, you'll see a huge list of all the types that are instances of this type class:

```haskell
-- ...
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Eq Word -- Defined in ‘GHC.Classes’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Eq Int -- Defined in ‘GHC.Classes’
instance Eq Float -- Defined in ‘GHC.Classes’
instance Eq Double -- Defined in ‘GHC.Classes’
instance Eq Char -- Defined in ‘GHC.Classes’
instance Eq Bool -- Defined in ‘GHC.Classes’
-- ... more instances
```

As you can see, all the types we encountered so far (and more) are instances of this type class (except for functions). That's why we can check if two values of type `Char`, `Int`, `Float`, etc are equal or not, and that's why we can apply the function `func` we just defined to any of them:


```haskell
func True False -- False

func 1 2        -- 2

func 1.0 1.0    -- 1.0

func 'a' 'c'    -- 'c'
```

And if you happen to pass a value that isn't an instance of `Eq`, like a function:


```haskell
f1 x = x + 1
f2 x = x + 2 - 1

func f1 f2
```

You'll get the error:

```haskell
No instance for (Eq (Integer -> Integer))
        arising from a use of ‘==’
        (maybe you haven't applied a function to enough arguments?)
```

Because, like the error says, the type `Integer -> Integer` is not an instance of `Eq`, and we needed to be because we're using `==`.

That's really cool, but you can't do much with types that belong only to the `Eq` type class. You can only tell if two values are equal or not. That's it. Luckily, `Eq` is not the only club in town!

### The `Ord` type class

The `Ord` type class is all about ordering. The types that are instances of the `Ord` type class can order their values and say which value is the biggest.

And for that, the `Ord` class has all these functions:

```haskell
  (<), (<=), (>=), (>) :: Ord a => a -> a -> Bool
  max, min             :: Ord a => a -> a -> a
  compare              :: Ord a => a -> a -> Ordering
```

We've already used the inequality operators ( `<`, `>`, `<=`, `>=`) in previous lessons. They take two values of the same type that belong to the `Ord` type class and return a boolean:


```haskell
4 > 9      -- False

'a' >= 'b' -- False
```

And how are the values ordered? It depends on the type. With numbers, it follows the mathematical order (e.g., `4` comes before `5` and after `3`). With characters, it follows the Unicode order. And other types have other rankings. As we said, each type that belongs to a type class has its own implementations (definitions) of those functions. We'll learn more about it when creating our own instances.

But with the ability to order things around, we can do more than just inequality.

#### The `min` and `max` functions

The `min` function takes two values of a type that is an instance of `Ord` and returns the minimum of the two:

```haskell
min :: Ord a => a -> a -> a
```

For example:


```haskell
min 12 19 -- 12
```

The `max` function takes two values of a type that is an instance of `Ord` and returns the maximum of the two:

```haskell
max :: Ord a => a -> a -> a
```

For example:


```haskell
max 12 19 -- 19
```

#### The `compare` function

The `compare` function takes two values of a type that is an instance of `Ord` and returns a value of type `Ordering`, indicating the order of the values.

```haskell
compare :: Ord a => a -> a -> Ordering
```

In the same way that `Bool` has only two values (`True` and `False`), the `Ordering` type has only three values: `LT` (lesser than), `EQ` (equal), and `GT` (greater than).

For example:


```haskell
compare 4 9         -- LT (4 is lesser than 9)

'f' `compare` 'e'   -- GT ('f' is greater than 'e')

True `compare` True -- EQ ( True is equal to True)
```

Again, so far, all the types we learned are instances of this class type (except for functions).

Now, you might say: "If I can check `EQ` with the `Ord` type class, why do I need the `Eq` type class?"

Sometimes a type has to first be an instance of one type class to be allowed to become an instance of another. Like you have to belong to the doodling club to be allowed to apply to the drawing club.

That's the case with `Eq` and `Ord`.

To order values of a type, for starters, you have to be able to tell if they are equal or not. This tells us that if we have a type that is an instance of `Ord`, it also supports all the `Eq` behavior! In these cases, we say that `Eq` is a superclass of `Ord` (conversely, `Ord` is a subclass of `Eq`).

Again, you don't need to memorize all these. Initially, you'll be able to quickly check it, and with a little bit of time, you'll know all the behaviors and subclasses by heart.

Something similar occurs with numeric type classes.

### The `Num` type class

Numeric types are one of the most used types in any programming language. But not all numeric types can do the same things.

Types that are instances of the `Num` type class can behave like numbers. But not like a specific subset of numbers. The `Num` type class defines the behavior that all numbers should have.

For example, types that are instances of this type class can be (among other things) added, subtracted, or multiplied:

```haskell
(+) :: Num a => a -> a -> a

(-) :: Num a => a -> a -> a

(*) :: Num a => a -> a -> a
```

For example:


```haskell
5 - 1      -- 4

8.9 + 0.1  -- 9.0

'a' - 'b'  -- ERROR! Char is not an instance of Num!

```

Now we're talking! Imagine I want to create a function that does some math:


```haskell
add1 x = x + 1
```

I don't want to choose a type like `Int` and only allow `Int` values. `Float`, `Double`, and `Integer` types could work perfectly fine! But, if there were no constraints, I could pass any type! What's the result of `'a' + 'b'`? Or `True + False`? It doesn't make any sense!

Because only types that are instances of the `Num` type class can use `+`, and because `Float`, `Double`, `Int`, and `Integer` are all instances of `Num`, we can constraint our function like this:


```haskell
add1 :: Num a => a -> a
add1 x = x + 1
```

But remember that if you're not sure of the type signature, ask the compiler! It knows that to use `+`, you have to be an instance of the `Num` type, so it infers the type signature of `add1` automatically! Providing flexibility and protecting us at the same time.

This is cool. But, sometimes, we need something more specific.

### The `Integral` type class

The `Num` type class includes all the numbers, but the `Integral` type class only the integral (whole) numbers. Such as `4`, but not `4.3`.

`Integral` is a more exclusive club than `Num`. Of all the types we saw so far, only `Int` and `Integer` belong to it.

This type class defines many behaviors, one of the most well-known `Integral` functions is `div`.

```haskell
div :: Integral a => a -> a -> a
```

It takes two values of a type that is an instance of `Integral` and divides them, returning only the whole part of the division.

Examples:


```haskell
3 `div` 5    -- 0

div 5 2      -- 2
```

And, on the flip side, we have the `Fractional` type class.

### The `Fractional` type class

The `Fractional` type class is all about fractional numbers. The types that are instances of the `Fractional` type class can represent and modify fractional values.

By far, the most used function that instances of the `Fractional` type class is `/`:

```haskell
(/) :: Fractional a => a -> a -> a
```

The all-mighty division. Unlike `div`, we can be more precise about our values because we're using fractional numbers. And only `Float` and `Double` are instances of this type class.

For example:


```haskell
10 / 5  -- 2.0

5  / 2  -- 2.5

10 / 3  -- 3.3333333333333335
```

Notice that we never had to specify the type of the numeric values in any of the examples so far. That's because, for example, the number `3` can be a value of type `Int`, `Integer`, `Float`, `Double`, and by applying certain functions, like `/`, the compiler can figure out that we meant the value `3` that belongs to one of the types that are instances of `Fractional`.


```haskell
:t (10/3) -- (10/3) :: Fractional a => a
```

### The `Show` type class

The `Show` type class is used to convert values to readable `String`s. It has 3 different behaviours, but the one you're going to see over and over is the `show` function:

```haskell
show :: Show a => a -> String
```

The `show` function returns a `String` representation of any type that is an instance of the `Show` type class. For example:


```haskell
show (3 :: Int) -- "3"

show True       -- "True"
```

This is really useful for debugging and printing logs.

### The `Read` type class

The `Read` type class provides the opposite behavior of the `Show` type class. Meaning it takes a `String` and returns a value of the type we ask for, if possible. The most often used behaviour is the `read` function:

```haskell
read :: Read a => String -> a 
```

For example:


```haskell
read "3" / 2  -- 1.5

read "True" || False  -- True

read "[1,2,3]" :: [Int]  -- [1,2,3]
```

Keep in mind that if the `String` doesn't contain a valid value or the `read` function doesn't know the type that needs to be returned, it will throw an exception:


```haskell
read "3" -- Doesn't know which numeric type. Exception.

read "Turue" :: Bool -- "Turue" is not a valid Bool value. Exception.
```

You can find a detailed description of a type class if you search it on Hoogle: https://hoogle.haskell.org/.

Now, let's take a look at how the compiler infers types.

## The most general valid type

What's the signature of this function?

```haskell
fToC x = (x - 32)*5/9
```

The `fToC` function could have a few different types. `fToC :: Float -> Float`, for example.

But, while doing type inference, the compiler assumes nothing and constrains the function's type as little as possible. Giving you the most general constraint.

Let's do it step by step.

So, in this case, the function takes a value and returns a value. So, the most general signature would be one:

```haskell
fToC :: a -> a  -- Intermediate step
```

But, the value it takes must be a numeric type (we're applying several mathematical functions. `-`, `*`, and `/`). 

But which type? `Num` (because of `-` and `*`) or `Fractional` (because of `/`)?

In this case, all numeric types are part of `Num`, but only `Float` and `Double` are part of `Fractional`. So, to make sure this function always works, it has to take the most restrictive type class, meaning `Fractional`:

```haskell
fToC :: Fractional a => a -> a
```

And that's how the compiler infers the type of the expression. Notice that the type could have been even more specific, like `Float -> Float` or `Double -> Double`. But that would be assuming you need a more constrained type without evidence.

At the end of the day, the most general valid type wins.

Ok, so, until now, we've been restricting if the type is an instance of a particular type class. And we know there can be more specialized type classes (`Fractional` is a more specialized type class than `Num`).

But what if we need a type with a more... particular set of abilities?

## Multiple constraints

Sometimes you need different constraints for different type variables.

Or the same type variable with multiple constraints. All this can be easily expressed in Haskell.

### Multiple constraints for the same type variable

Take this function that skips the number 3:


```haskell
skip3 x = if x == 3 then x+1 else x
```

The `x` can be of any type that is an instance of `Eq` (because of `==`) and `Num` (because of `+` and because we're comparing the input with the value `3` that belongs to the `Num` type class).

To specify multiple constraints for the same type variable, we have to surround them with parenthesis and add a comma between them.

Like if they were a tuple:

```haskell
skip3 :: (Eq p, Num p) => p -> p
```

Now the `p` type variable has to be a type that's an instance of both `Eq` and `Num`. And, of course, we could add more constraints if needed.

### Constraints for multiple type variables

Let's create a function that takes two values and returns `1` if the first value is greater than the second and `0` otherwise:


```haskell
isXBigger x y = if x > y then 1 else 0
```

In this case, `x` and `y` have to be instances of `Ord`. And the return value is a number of an unspecified type, so it's the more general `Num` instance.

Putting this together, the type signature will be:

```haskell
isXBigger :: (Ord a, Num p) => a -> a -> p
```

Now, let's practice just a bit. What about this function?:


```haskell
mistery1 x y z = if x > y then z/2 else z
```

We compare `x` and `y` with `>`, so they have to be instances of the `Ord` type class.

And the return value is divided using `/` in one of the if-else paths. So `z` has to be an instance of `Fractional`.

```haskell
mistery1 :: (Ord a, Fractional p) => a -> a -> p -> p
```

And finally, our last example is a modification of `mistery1` where we add `1` to `x` before comparing it to `y`:


```haskell
mistery2 x y z = if x+1 > y then z/2 else z
```

Same as before. But now `x` and `y` also have to be an instance of `Num` to be able to use `+`:

```haskell
mistery2 :: (Ord a, Num a, Fractional p) => a -> a -> p -> p
```

As you can see, **we can apply as many constraints as needed**.

Of course, in the day-to-day, the compiler can infer them for you (most of the time). But you'll still have to be aware of what's going on to correctly interpret and understand them. Also, writing a function's type before defining it is a good practice and a great way to ease up the process of defining it later.

# That's it for today!
