```haskell
:opt no-lint
```

# Improving and combining functions

## Outline

- Higher-order functions
	- `filter`
	- `any`
- Lambda functions
- Precedence and associativity
- Curried functions
    - Partial application
- Applying and composing functions 
	- The `$` operator
	- The `.` operator
- Point-free style

## Higher-order functions

A **higher-order function** is a function that takes other functions as arguments or returns a function as a result. 

Because we can pass functions as an input, return them as a result, and assign them to variables, they are like any other value. So we say that functions are **first-class citizens**.

Let's start with a classic example. Imagine that you have a function you usually apply twice (for some reason). Like this:


```haskell
complexFunc1 :: Int -> Int
complexFunc1 x = x + 1

func1 :: Int -> Int
func1 x = complexFunc1 (complexFunc1 x)

complexFunc2 :: Int -> Int
complexFunc2 x = x + 2

func2 :: Int -> Int
func2 x = (complexFunc2 (complexFunc2 x)) + (complexFunc2 (complexFunc2 x))
```

This is an exaggerated example, but you can see how a pattern starts to emerge. You always use the `complexFunc1` and `complexFunc2` twice! As soon as we saw this pattern, we realized we could do better. What if we create a function that takes two parameters–a function and a value–and applies the function to the value twice!

We can do that by simply writing:


```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

Here, the type signature is different from previous ones. The `(a -> a)` part indicates that the first parameter is a function that takes a value of type `a` and returns a value of the same type. The second parameter is just a value of type `a`, and the whole `applyTwice` function returns a value of type `a`.

And in the body of the function, you can see that it takes the first parameter (the function `f`), applies it to `x`, and then applies `f` again to the result. So we're applying the function `f` twice.

And that's it! We created a higher-order function!

We can use the `applyTwice` function to simplify the previous code like this:


```haskell
func1' :: Int -> Int
func1' x = applyTwice complexFunc1 x

func2' :: Int -> Int
func2' x = (applyTwice complexFunc2 x) + (applyTwice complexFunc2 x)
```

This is a simple example, but higher-order functions are an extremely powerful feature. So much so that they are everywhere! In fact, you could create your own Domain Specific Language using higher-order functions! But let's take it step-by-step. Let's start by using two higher-order functions that come with Haskell.

### `filter` function

Let's start with the `filter` function:


```haskell
:t filter 
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
</style><span class='get-type'>filter :: forall a. (a -> Bool) -> [a] -> [a]</span>


This function takes a predicate (a function that returns a boolean) `a -> Bool` and a list of elements of type `a` and filters the elements of the list given the predicate.

For example, if we want to filter only the even numbers from a list of 1 to 20, we could do something like:


```haskell
filter even [1..20]
```


    [2,4,6,8,10,12,14,16,18,20]


Or, for a more involved condition, we could filter from a list of fruits only the ones that contain the letter `'a'`:


```haskell
fruitWithA = filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
                where tempFunct x = 'a' `elem` x
fruitWithA
```


    ["Banana","Pear","Grape"]


As you can see, you can also define a function in a `where` clause to pass it as the predicate of the `filter` function.

### `any` function

We also have the `any` function:


```haskell
-- Only for lists:  any :: (a -> Bool) -> [a] -> Bool
```

This function also takes a predicate and a list of elements. But this one checks if there exists **any** element in the list for which the predicate holds.

For example, here we're checking if any of the elements of the list is greater than 4. If only one is, `any` returns `True`, else, it returns `False`:


```haskell
biggerThan4 x = x > 4

any biggerThan4 [1,2,3,4] 
```


    False


A more realistic way to use `any` would be to check if we have any cars left on our car-selling website:


```haskell
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

biggerThan0 (_,x) = x > 0

any biggerThan0 cars
```


    True


In `biggerThan0`, we're pattern matching on the tuple to extract the number of cars and check if it's greater than zero. Then, we're using `any` to check if any of all the pairs in the list has at least one car left.

Ok, we saw plenty of examples of functions that take other functions as parameters. But what about functions that return functions as results? We'll get there. First we'll learn about lambda abstractions and curried functions.

## Lambda functions

The term lambda function comes from the mathematical system called **lambda calculus**. It's an intriguing and powerful subject by itself, but today, we're going to look at it from the practical-programmer point of view.

A lambda function (also called anonymous function) is a function definition that doesn't have a name.

For example, here's how a lambda function that takes two arguments and multiplies them ( $f(x,y)=x*y$ ) looks in Haskell:

```haskell
\x y -> x * y
```

A lambda function consists of four things:

 1. The backslash `\` at the beginning tells us that this is a lambda function.
 2. The parameter names (in this case `x y`) that the function takes as inputs.
 3. The arrow (`->`) that **separates** the inputs from the body.
 4. And everything after the arrow that is the **body** of the function.

<div class="alert alert-block alert-info">
Most modern programming languages also have anonymous functions. But not all of them work the same way.
</div>

### Why should you care?

It sounds useless because how can you use a name-less function? You have no way of calling it later!

Actually, it's a powerful component of the language! Through this course, we'll encounter many situations where lambda expressions are practical. For starters, you can use lambda expressions to avoid naming functions that you'll only use once!

This is useful in and of itself, but it really shines when working with higher-order functions! For example, take a look at the previous example:


```haskell
biggerThan4 x = x > 4

any biggerThan4 [1,2,3,4] 
```


    False


That `biggerThan4` function won't be used anywhere else, but it will linger in our environment forever. Also, it's an awfully simple function! The name is longer than the body!

By using lambda expressions, we can create and use `biggerThan4` as a parameter for `any` all at the same time like this:


```haskell
any (\x -> x > 4) [1,2,3,4]
```


    False


We can also use lambda expressions to simplify other functions. Let's review the `fruitWithA` function:


```haskell
fruitWithA = filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
                where tempFunct x = 'a' `elem` x
fruitWithA
```


    ["Banana","Pear","Grape"]


We can simplify `fruitWithA` by removing the `tempFunct` and replacing it with a lambda function:


```haskell
filter (\x -> 'a' `elem` x) ["Apple", "Banana", "Pear", "Grape", "Wood"]
```


    ["Banana","Pear","Grape"]


And, of course, because lambda functions are just expressions, you could use them anywhere an expression can be used. Even by themselves:


```haskell
(\x -> x*2 + 1) 3
```


    7


If you need more examples, keep watching/reading. Lambda functions will be a valuable tool to easily visualize currying.

Right now, we'll take a few minutes to learn about precedence and associativity.

## Precedence and associativity

### Precedence

Precedence indicates the priority of an operator (denoted by a number from 0 to 9). If we use two operators with different precedence, the one with the higher precedence gets applied first. Meaning that higher precedence operators bind more tightly!

We can get the precedence for an operator with the info command `:i`. 


```haskell
:i (+)  -- infixl 6 +
:i (*)  -- infixl 7 *

1 + 2 * 3  -- Same as 1 + (2 * 3)
```






    7


<div class="alert alert-block alert-info">
    <code>infixl 6 +</code> and <code>infixl 7 *</code> are called <b>fixity declarations</b>.
</div>

Because multiplication has precedence of 7, which is higher than the addition's precedence of 6, the result is 7 and not 9.

And what happens when two operators have the same precedence? This is when associativity comes into play.

### Associativity

When we used the `:i` command previously, it also returned the keyword `infixl`. This is the operator's associativity.

When two operators have the same precedence, the associativity tells you which side (left with `infixl` or right with `infixr`) will be evaluated first.

For example:
- The operators `(+)` and `(*)` have left associativity, which means they evaluate the left side first.
- The `(:)` operator has right associativity, which means it evaluates the right side first.
- The `(==)` operator has no associativity (`infix`), which means that, if you use more than one, you need parenthesis to indicate the order.


```haskell
1 + 2 + 3 + 4  -- infixl: Same as ((1 + 2) + 3) + 4

1 : 2 : 3 : [] -- infixr: Same as 1 : (2 : (3 : []))

True == (False == False) -- infix: If you remove parenthesis, you'll get an error.
```


    10



    [1,2,3]



    True


And, of course, you can change the evaluation order using parenthesis:


```haskell
:i (**) -- infixr 8 **

2**3**4  -- infixr: Same as 2 ** (3 ** 4)
(2**3)**4
```

Finally, we can define precedence and associativity when creating our own operator. Like this:


```haskell
x +++ y = x + y -- Creating +++ operator
infixl 7 +++    -- Setting fixity of operator

1 +++ 2 * 3  -- 9
```


    9


Now, the result is 9 because `+++` and `*` are both left-associative and have the same precedence.

<div class="alert alert-block alert-info">
<b>Important note:</b> 
   <ul>
       <li>Operators without an explicit fixity declaration are <code>infixl 9</code></li>
       <li>Function application (the "whitespace operator") always has the highest precedence (imagine precedence 10).</li>
   </ul>
</div>

## Curried functions

Currying is the process of changing a function so that rather than taking multiple inputs, it takes a single input and returns a function which accepts the second input, and so forth.

And here's the kicker:

**In Haskell, all functions are considered curried! That is, all functions in Haskell take just one argument!**

To exemplify this, take a look at this function:


```haskell
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
```

It seems like a multi-parameter function. But!, there are hidden associativities in play! We know that function application (the "whitespace operator") always has the highest precedence and associates to the left, so if we make that obvious, we get:


```haskell
add3 :: Int -> Int -> Int -> Int
((add3 x) y) z = x + y + z
```

And if we check the fixture of the function arrow (`->`):


```haskell
:i (->)  -- infixr -1 ->
```



We see that it associates to the right! So, a more explicit way of writing the signature of the `add3` function is:


```haskell
add3 :: Int -> (Int -> (Int -> Int))
((add3 x) y) z = x + y + z

add3 1 2 3
```


    6


Which perfectly corresponds with the function's definition! But, just to make it painfully obvious, we'll make currying explicit using lambda functions.

Starting with the previous definition:


```haskell
add3 :: Int -> (Int -> (Int -> Int)) -- Same as: add3 :: Int -> Int -> Int -> Int
((add3 x) y) z = x + y + z           -- Same as: add3 x y z = x + y + z
```

We'll move each parameter from the left side of the `=` sign to the right side. Creating the same function several times but written differently. So, starting with `z` (the outermost parameter), an equivalent `add3` function that does exactly the same as the original can be written like this:


```haskell
add3 :: Int -> (Int -> (Int -> Int))
(add3 x) y = \z -> x + y + z
```

Now, `add3` is a function that takes two numbers (`x y`) and returns a function that takes another number (`z`) and adds the three together.

If we do it again for the second value:


```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 x = \y -> (\z -> x + y + z)
```

Now, `add3` is a function that takes one number (`x`) and returns a function that takes one number (`y`) that returns a function that takes one number (`z`) and adds the three together.

And if we do it one more time:


```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 = \x -> (\y -> (\z -> x + y + z))
```

We get that `add3` is a name that returns a function that takes one number (`x`) and returns a function that takes one number (`y`) that returns a function that takes one number (`z`) that adds the three numbers together.

That was quite a journey, but we managed to make currying explicit!

And now, the way signatures are written makes way more sense! Each time you replace one parameter, it returns a new function as a result. That is until you replace the final one that gives you the final result.

And because `->` is right-associative, we can remove the use-less parentheses of both the signature and definition to get a cleaner code:


```haskell
add3 :: Int -> Int -> Int -> Int
add3 = \x -> \y -> \z -> x + y + z
```

And now, for example, if we apply the function to 3 parameters like this:


```haskell
add3 1 2 3 
```


    6


This is what happens step by step (I added the parentheses for visual aid):

```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 = \x -> (\y -> (\z -> x + y + z)) 

---

add3 1 = \y -> (\z -> 1 + y + z)       :: Int -> (Int -> Int)

add3 1 2 = \z -> 1 + 2 + z             :: Int -> Int

add3 1 2 3 = 1 + 2 + 3                 :: Int
```

So, besides being a cool conversation starter at the club, how is this useful to you? Well... with uncurried functions, if you provide fewer parameters than the ones required, you get an error. But because, in Haskell, all functions are curried, you can take advantage of it to use partial application!

### Partial application

Partial application in Haskell means that you provide fewer arguments than the maximum amount the function accepts.

The result (like we saw earlier) is a new function that takes in the rest of the parameters you did not provide to the original function. 

As a practical example of how this is useful, let's say you have a function used to create an email in the format `name.lastName@domain`. The parameters you provide are the domain, the name, and the last name:


```haskell
createEmail :: String -> String -> String -> String
createEmail domain name lastName = name ++ "." ++ lastName ++ "@" ++ domain
```

Now, your company has two communities as clients, which have two different domain names. You don't want your users to write out the domain name every time, so you create 2 functions where you partially apply their domain names:


```haskell
createEmailTeckel :: String -> String -> String
createEmailTeckel = createEmail "teckel-owners.com"

createEmailSCL :: String -> String -> String
createEmailSCL = createEmail "secret-cardano-lovers.com"

createEmailTeckel "Robertino" "Martinez"
createEmailSCL "Vitalik" "Buterin"
```


    "Robertino.Martinez@teckel-owners.com"



    "Vitalik.Buterin@secret-cardano-lovers.com"


Notice that this is possible because the domain is the first parameter in the function `createEmail`. So the order of the arguments matters.

If, for some reason, the parameter you want to apply is not the first one and you are not allowed to rewrite the existing function, you can create a helper function:


```haskell
-- With partial application:

createEmailJohn :: String -> String -> String
createEmailJohn lastName domain = createEmail domain "John" lastName

-- Without partial application:

createEmail' :: String -> String -> String -> String
createEmail' name lastName domain = createEmail domain name lastName
```

And because operators are just infix functions, we can also partially apply them!

For example, recalling the previous example of a higher-order function:


```haskell
any (\x -> x > 4) [1,2,3,4]
```


    False


In the function we pass as a parameter, we need to compare if the input is larger than `4`. And the `>` operator is already a function that takes two parameters and compares if the first is larger than the second. So we can partially apply the parameter on the right to get the same result:


```haskell
any (>4) [1,2,3,4]
```


    False


The partial application of an infix operator is called a *section*.

And I'm not sure if you noticed, but we just replaced the second parameter (the one on the right). The cool thing about sections is that you can partially apply the more convenient side:


```haskell
(++ "ing") "Think"     -- Same as \x -> x ++ "ing"

("Anti" ++) "library"  -- Same as \x -> "Anti" ++ x
```


    "Thinking"



    "Antilibrary"


<div class="alert alert-block alert-warning">
<b>Warning:</b> The <code>-</code> operator is special because you can't partially apply it. <code>-1</code> is parsed as the literal <code>-1</code> rather than the sectioned operator <code>-</code> applied to <code>1</code>. The <code>subtract</code> function exists to circumvent this issue.
</div>

## Applying and composing functions 

### The function application `$` operator

If we check how the function application operator is defined in Haskell, it seems a little... weird:

```
($) :: (a -> b) -> a -> b
f $ x =  f x
```

We see that it takes in a function `f` and a variable `x` and then applies the function to the variable (`f x`). So, it looks like this operator is redundant since it does the same as an ordinary function application (`f x`). 

And, you know what? It is! However, there's a small but significant difference between the two:

- The "white space" operator has the highest left-associative precedence.
- The function application operator (`$`) has the lowest right-associative precedence: `infixr 0 $`.

You can see the difference if we make this evident using parenthesis:

```
f g h x      = ((f g) h) x

f $ g $ h x  =  f (g (h x))
```

As an example of how this changes things, take a look at the following expressions:


```haskell
(2 *) 3 + 4    -- Same as: ((2 *) 3) + 4
(2 *) $ 3 + 4  -- Same as: (2 *) (3 + 4)

max 5 4 + 2    -- Same as: ((max 5) 4) + 2
max 5 $ 4 + 2  -- Same as: (max 5) (4 + 2)
```

As you can see in the previous examples, when using `$`, the whole expression on its right is applied as the parameter to the function on its left. So you can see how using `$` is like surrounding everything to its right between parentheses.

This brings us to the primary use of `$`: Omitting parentheses!

In the following expression, there are 3 opportunities to remove parenthesis, so we remove them:


```haskell
-- All these expressions are equivalent:

show ((2**) (max 3 (2 + 2)))

show $ (2**) (max 3 (2 + 2))

show $ (2**) $ max 3 (2 + 2)

show $ (2**) $ max 3 $ 2 + 2
```

This makes your code more readable and easy to understand.

Of course, you can do more than remove parenthesis, but that's what you'll do most of the time. So we'll leave it there and start learning about the function composition operator (`.`)!

### Function composition

We already covered the concept of function composition in our first lesson. So if you're not sure about it, go check it out! But, just a refresher and in a few words: 

When we compose two functions, we produce a new function that is the equivalent of calling the two functions in sequence when the first one takes the output of the second one as input.

We could do this with parenthesis. Here, the function `f` takes as input the result of applying the function `g` to `x`:

```haskell
f (g x)
```

As a probably overcomplicated example, we could do something like this:


```haskell
complicatedF :: [Int] -> Bool
complicatedF x = any even (filter (>25) (tail ( take 10 x)))
```

Here, we're composing quite a lot! 3 times to be exact! And as you can see, this is quite hard to read, so a diagram could help:

$$
    \boxed{\mathrm{[Int]}}
        \xrightarrow{\mathrm{~~~~~~take~10~~~~~~}}
    \boxed{\mathrm{[Int]}}
        \xrightarrow{\mathrm{~~~~~~tail~~~~~~}}
    \boxed{\mathrm{[Int]}}
        \xrightarrow{~~~~~~\mathrm{filter~(>25)}~~~~~~}
    \boxed{\mathrm{[Int]}}
        \xrightarrow{~~~~~~\mathrm{any~even}~~~~~~}
    \boxed{\mathrm{Bool}}
    \\
    \
    \\
    \
    =
    \
    \\
    \
    \\
    \boxed{\mathrm{[Int]}}
        \xrightarrow{\mathrm{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~complicatedF~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}}
    \boxed{\mathrm{Bool}}
$$

We take a list of `Int` as input, then use `take 10` to take the first 10 elements of the list, then use the result as an input for `tail` that returns the last 9 elements, then use the result of that as an input for `filter (>25)` to filter the values greater than 25, and finally, take the result of that as an input for `any even` to check if there are any even numbers left in the list.

The diagram helped, but what if I tell you there's a way to have something as clean and easy to understand but in our code? 

This can be done by abstracting function composition to an operator. And because, in mathematics, the composition symbol is a ring that kind of resembles a dot, we'll use a dot:

```haskell
(.)  :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
infixr 9 .
```

Here, we see that the `.` operator takes two functions (`f :: b -> c` and `g :: a -> b`) and composes them using a lambda function to indicate that the whole `f . g` expression returns a function that takes the parameter `x :: a`, applies `g` to it to get a value of type `b`, and finally applies `f` to it to get a value of type `c`.

It's important to notice that `f` takes as input a value that has the same type as the output of `g`. So the resulting function takes as input a value of the same type as `g`'s input (`a`) and returs as output a value of the same type as `f`'s output (`c`).

So, now that we have this new operator, how does the `complicatedF` function look now? Like this:


```haskell
complicatedF :: [Int] -> Bool
complicatedF x = any even . filter (>25) . tail . take 10 $ x
```

Waaay more readable! You can tell everything the function does with a quick glance!

Also, notice that every function to both sides of the `.` operator takes a single argument or is partially applied until it takes a single argument.

If we rewrite the example from the application operator chapter by using the dot operator, we get:


```haskell
show ((2**) (max 3 (2 + 2)))

show . (2**) . max 3 $ 2 + 2
```

As you can see, `$` and `.` can make your code clear and concise. But be wary to not overuse them! You could end up having a worst result!

And now, as a final way to make your functions more readable, ladies and gentlemen, we present the point-free style!! 👏👏👏

### Point-free style

In point-free style (also called tacit programming), function definitions don't declare the arguments.

So, instead of doing this:


```haskell
fourOrLarger :: Int -> Int
fourOrLarger x = max 4 x

add1 :: Int -> Int
add1 x = 1 + x
```

We can do this:


```haskell
fourOrLarger :: Int -> Int
fourOrLarger = max 4

add1 :: Int -> Int
add1 = (1+)
```

The functions do the same, but now, we're not explicitly binding the argument and using it inside the body. That's implicit in the definition but still explicit in the signature.

Pont-free functions have the advantages of:
- Being more compact.
- Easier to understand.
- Cleaner, since they discard redundant information.

So, we can use the point-free style to change this:


```haskell
complicatedF :: [Int] -> Bool
complicatedF x = any even . filter (>25) . tail . take 10 $ x
```

Into this:


```haskell
complicatedF :: [Int] -> Bool
complicatedF = any even . filter (>25) . tail . take 10
```

This gives us our final expression of `complicatedF`.

This style is particularly useful when deriving efficient programs by calculation and, in general, constitutes good discipline. It helps the writer and reader think about composing functions at the high level instead of shuffling data at the low level.

This concludes today's lesson. Today we learned plenty of new concepts and ways to improve and combine our functions. It may be a lot to take in at once, but all these concepts are important. So make sure you understand them well before advancing with the course.
