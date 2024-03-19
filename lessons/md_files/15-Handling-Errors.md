# Handling🧑‍🚒🧯💨 🔥errors🔥 in Haskell

Haskell is a compiled language, so we can subdivide all possible errors into:

- ✅ Compile time errors 🙌

Compile time errors are awesome! We LOVE compile-time errors. Because that means our program had something wrong in it, and our compiler found it and reported it to us **BEFORE** it reached the end user. At the end of the day, the most important thing is that our users enjoy our software and don't encounter:

- ❌ Run time errors 🫣

Runtime errors are the worst! Because these errors happen while running the program, they can and do, happen to end users. Users then get angry and stop paying, give us a 1-star review, or something like that.

Compared to most other programming languages, Haskell is exceptionally good at avoiding runtime errors. To the point that many say the phrase, "in Haskell, if it compiles, it works."

This is thanks, in large part, to its purity and powerful type system that can catch those errors at compile time. Essentially, moving many errors from runtime to compile time.

Although, this doesn't mean that we can forget about runtime errors. As we said in previous lessons, even if we were able to write perfect code (which we cannot), we're still running it in the real world. Where computers run out of memory, files that should exist don't, internet connections fail, etc. And on top of that, users do unimaginable things with our software.

So, in this lesson, we will learn both how to handle runtime errors and how to use the type system to move some of those errors to compile time so we catch them with the compiler's assistance.

More specifically:

# Outline

- There're always `Exception`s to the rule
- Speed-running `Exception`s with a dumb self-driving 🤖 car 🚗
    - I'm the `Exception` cause I have `class` 😎
    - `throw` all the `Exception`s you want. I'll `catch` them all!
- `Maybe` give me a value? 🙏
    - Benefits of optional values
- Ok, you `Either` give me a value or a reason why you didn't!
- From `Exception`s to optional values
- Tradeoffs
    - So, what should I use?

Let's get started!

## There're always `Exception`s to the rule

Let's write a simple program to calculate the velocity of an object based on a number written in a file and the input of a person:

```haskell
calcVel :: IO ()
calcVel = do
  d <- readFile "aNumber.txt"
  putStrLn "Loaded the distance traveled by the object."
  putStrLn "Provide the time it took:"
  t <- getLine
  let v = (read d :: Int) `div` read t
  putStrLn $ "The object's velocity is about: " ++ show v
  putStrLn "Thank you for using this program!"
```

In this program, we read a file (the distance traveled by an object) and ask the user to provide a number (the time it took) using IO actions. Then, we use the `read` function to parse them into `Int` values and apply the `div` function to get the velocity of the object. In the end, we print back the velocity and a thank-you message.

This is a pretty simple program. It compiles, and if we have a file in the `aNumber.txt` with a number in it, and the user provides a valid number, it works!!

Sadly, the real world is messy, and unexpected things can–and do–happen. When something unexpected happens, that's when we get an `Exception`. For example, here are a few (but not all) of the possible `Exception`s that the program could throw at us:

If there's no `aNumber.txt` file:
```
*** Exception: aNumber.txt: openFile: does not exist (No such file or directory)
```

If we don't provide valid numbers to `read`:
```
*** Exception: Prelude.read: no parse
```

If we provide `0`(zero) as time:
```
*** Exception: divide by zero
```

Currently, all these `Exception`s cause our program to crash. But, of course, a robust program should not collapse if something unexpected happens. Luckily, we have a couple of ways to handle these `Exception`s.

## Speed-running `Exception`s with a dumb self-driving 🤖 car 🚗

Handling exceptions in Haskell is not a small subject. It has its own mechanism, and you can go crazy creating your own hierarchical error structures, using alternative libraries, etc. But you have more important things to learn before that. So, in this lesson, we're going to speedrun the subject in a way you'll have an overall idea of how it works, and you'll be able to handle common runtime errors.

Ok. Let's say we want to build an AI for a self-driving car. AI is all the rage right now, so we're going to build a prototype to get some VC funding and improve it later.

To simplify the problem, we'll start with a car that can only go straight and react to traffic lights:

```haskell
dumbAICar :: IO ()
dumbAICar = do
  putStrLn "What color is the traffic light?"
  color <- getLine
  putStrLn $ "\nThen I'll " ++ nextMove color
  dumbAICar
```

In these complex cases, it's usual to have several different systems, sensors, and predictive models. But! Because no one invested in our AI yet, we don't have money to spend on a sensor. So we'll be the sensor. We'll sit inside the car and type the color of the traffic light.

Once we have a `String` containing the color, we provide it to the `nextMove` function that eventually should send a signal to the gas and brakes. For now, it prints the next move on the screen.

Now, let's create the `nextMove` function. We're taking a `String` that represents the color of traffic lights, so we could do something like:

```haskell
data TrafficLight = Red | Yellow | Green deriving (Show, Read)

nextMove :: String -> String
nextMove color = case read color of
  Red    -> "Stop!"
  Yellow -> "Waaaait..."
  Green  -> "Go!"
```

This is technically right. But we have two problems:

1. If we write something different than any of those three constructors, our program halts, and subsequently, the car crashes with us inside.
2. And the other one is that the error provided in the case it fails doesn't provide much actionable information:

```
*** Exception: Prelude.read: no parse
```

Let's solve the second issue first by providing a more expressive exception.

### I'm the `Exception` cause I have `class` 😎

To create our own `Exception`, we just need create a good old data type, and make it an instance of the `Exception` type class.

So, lets create a data type that represents the possible failures:

```haskell
data TrafficLightException = TrafficLightIsOff | WrongColor String
    deriving (Show)
```

One value for when the traffic light is off, and another when the sensor provides values that don't make sense. Now, this is still what we call "normal" values. To make them exceptional, we have to import the `Control.Exception` module:

<div class="alert alert-block alert-info">
<b>Note:</b> We have to import <code>Control.Exception</code>, which is the standard library module that contains code related to exceptions. 
</div>

And make the type an instance of `Exception` like this:

```haskell
instance Exception TrafficLightException
```

Notice we don't define the behaviors with the `where` keyword. That's because we don't have to! All the `Exception` type class functions have default implementations, so we only need to indicate that the `TrafficLightException` can use those.

This conveniently allows us to use exceptional values without entering into how the `Exception` type class is defined and the needy-greedy of the mechanism. Things that would take too long to properly go over and would touch subjects that are a bit outside of the beginner's scope.

So, yeah! That's all we need to do! With that single line of code, we transformed our "normal" type into an "exceptional" type. That means we can throw and catch values of type `TrafficLightException` like any other built-in exception.

But what does that mean exactly?

### `throw` all the `Exception`s you want. I'll `catch` them all!

Throwing exceptions means halting the current program's execution and sending a signal with some information about what went wrong. That signal is the `Exception`.

We have a couple of options to throw exceptions. We're going to go with:

```haskell
throwIO :: Exception e => e -> IO a
```

As the name and type suggest, `throwIO` gets a type that's an instance of the `Exception` type class and throws an exception within the `IO` context. We return a value within the impure `IO` context because exceptions can have side effects.

Technically, you can throw exceptions from pure code using a function called `throw` (without the IO). But it's highly recommended to use `throwIO` to maintain our pure code exception-free.

Ok, let's change the `nextMove` function to manually parse the `String` instead of using the `read` function and throw as many exceptions as we want:

```haskell
nextMove :: String -> IO String
nextMove color = case color of
  "Red"    -> return "Stop!"
  "Yellow" -> return "Wait!"
  "Green"  -> return "Go!"
  "Black"  -> throwIO TrafficLightIsOff
  _        -> throwIO . WrongColor $ color ++ " is not a valid color!"
```

And here's when we encounter one of the downsides to throwing exceptions. We don't want to throw from pure code (because, due to laziness, it is hard to predict), so we throw from the IO context. This means our previously pure `nextMove` function is now impure, and we lose all the advantages that come with purity. We'll talk more about this at the end of the lecture.

So! Instead of using `read`, we parse the `String` by hand and provide extra cases to handle our newly created exceptions. 

Specifically, if we apply `nextMove` to one of the two final patterns, instead of returning a value, we throw an exception:

```haskell
>> nextMove "Black"
*** Exception: TrafficLightIsOff


>> nextMove "Rainbow"
*** Exception: WrongColor "Rainbow is not a valid color!"
```

This solves the issue of not providing enough information because we can add custom messages that help when debugging the program. Keep in mind that this function can be buried inside a massive program. But because the exception is so specific, we instantly know what's wrong and where we should go to fix it.

But, if we leave it like this, we just created a fancier way for our program to halt. 🤦‍♂️ To make our program more resilient, we're going to catch, handle, and recover from this exception! 💪

Same as with `throwIO`, there are several ways to catch and handle an `Exception`. One of them is using the `catch` function:

```haskell
catch :: Exception e  -- Type e is an instance of Exception
      => IO a         -- The computation to run
      -> (e -> IO a)  -- Handler to invoke if an exception is raised
      -> IO a         -- We always return a value of type IO a
```

This function executes the `IO` action that we provide as the first parameter. If everything goes well, it returns the result, and that's it. BUT! If some exception arises, it catches it and gives it to the handling function we provide as a second argument. In that handling function, we can write all sorts of code to address this exception and resume our program.

For example, we can use `catch` to handle our `TrafficLightException` like this:

```haskell
dumbAICar :: IO ()
dumbAICar = do
  putStrLn "What color is the traffic light?"
  color    <- getLine
  response <- nextMove color `catch` handler
  putStrLn $ "I'll " ++ response
  dumbAICar
  
  where
    handler :: TrafficLightException -> IO String
    handler e = do
      -- do whatever you want...
      putStrLn $ "WARNING: " ++ show e
      case e of
        TrafficLightIsOff -> return "Proceed with caution."
        WrongColor _      -> return "Stop the car and shut down!"
```

Now, it doesn't matter if the String isn't a valid color. The `nextMove` function will raise an exception. And, because the exception is of type `TrafficLightException`, the `dumbAICar` action will be able to catch it and give it to our `hanlder` to recover from it.

In this case, if the traffic light is off, the car will proceed with caution. And if we happen to encounter any other color, that means something is wrong with the sensor. But instead of halting the program and leaving a moving car without a driver, the program continues execution and stops the car.

<pre>
<span style="color:blue;">>></span> dumbAICar
What color is the traffic light?
<span style="color:blue;">></span> Red
I'll Stop!
What color is the traffic light?
<span style="color:blue;">></span> Green
I'll Go!
What color is the traffic light?
<span style="color:blue;">></span> Black
<b>WARNING: TrafficLightIsOff</b>
I'll Proceed with caution.
What color is the traffic light?
<span style="color:blue;">></span> Rainbow
<b>WARNING: WrongColor "Rainbow is not a valid color!"</b>
I'll Stop the car and shut down!
</pre>

Now, at least we won't crash if something's wrong with the sensor.

If the exception is of any other type, it will keep propagating up the stack looking for a `catch` that can handle that specific type. If it doesn't find any, the program won't be able to recover.

And that's the end of our speedrun. There are a ton of things we didn't cover, but everything revolves around this notion of throwing and catching exceptions. If you can foresee an exception and know what the program should do in that case, you can implement a handler to recover from it.

And how do we know the usual exceptions we could encounter? Look no further than in the same `Control.Exception` module from where we imported the `Exception` type class.

For example, if you're dealing with numeric operations, you can take a look at the `ArithException` data type:

```haskell
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator 
```

And if you're dealing with Arrays, you might encounter an exception of type `ArrayException`:

```haskell
data ArrayException
  = IndexOutOfBounds  String -- Indexed an array outside its bounds
  | UndefinedElement  String -- Evaluated an uninitialized element
```

Etc, etc. 

In all those cases, all you have to do is catch the exception of a specific type and provide a handler function to recover from it. There may be no sensible way to continue. In those cases, we want to shut down gracefully. By shutting down gracefully, I mean cleaning up any open connections, killing orphan processes, writing some logs, etc. It depends on what you're doing.

Now... The exceptions mechanism is great, but there's also a different way to handle runtime errors, and that is more straightforward and idiomatic. An that's using optional values.

## `Maybe` give me a value? 🙏

In Haskell and other functional programming languages, we have this notion of "opotional values." Those are values representing the possibility that a function may or **may not** return a meaningful value.

In Haskell, optional values are represented by the `Maybe` type:

```haskell
data Maybe a = Nothing | Just a
```

As you can see, it's a simple type. It has one nullary constructor (a constructor that doesn't have parameters) and one constructor with a polymorphic value. The major implementation difference with the other way to handle `Exceptions`, is that `Maybe` is a "normal" type. It's not special in any way other than what we (the developers) interpret by it.

So, the critical thing to remember is how to interpret it when we encounter it. The `Maybe` type represents a value that might or might not be there. So, when you see something like this:

```haskell
someFunction :: Int -> String -> Maybe Bool
```

We read it as:

`someFunction` takes an `Int` and a `String`, and `Maybe` returns a `Bool`.

That's it.

Now, let's see it in practice. One common example is to show how we can safely divide by zero.

In your regular day-to-day math, dividing by zero doesn't make any mathematical sense. When programming, it's the same. So, if we divide by zero using the `div` function like this:

```haskell
55 `div` 0

*** Exception: divide by zero
```

Our program throws an Exception.

Let's solve that without using the exception mechanism.

One of the first things one could think to resolve the issue is to create a function that handles this specific case separately, sort of like this:

```haskell
safeDiv :: Integral a => a -> a -> a
safeDiv x 0 = ????? -- 0? -1? 9999999999999? 
safeDiv x y = x `div` y
```

The `saveDivide` function takes two values that are instances of the `Integral` type class (the one that provides the `div` behavior) and returns a value of the same type. In the first case, we pattern-match to handle the case when the denominator is zero. And in the second, all other cases. The second case is easy... we know that `y` is not zero, so we can use `div` with full confidence. But how should we handle the first case? There's no number we could return that correctly represents this situation!


Here's when the `Maybe` type shines! Using `Maybe`, we can modify our function to have the option of not returning a value at all!!:


```haskell
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)


15 `safeDiv` 3
15 `safeDiv` 0
```


    Just 5



    Nothing


As you can see, if there's no sensible value to be returned, we can plug the `Nothing` value. Notice that by avoiding the possibility of dividing by zero from the get-go, we bypass the exception altogether! No need to handle exceptions if they never happen!

The price that we have to pay is that the value is now wrapped around a constructor. So we have to unwrap it before using it.

Now, let's use the `safeDiv` function in a program. To do that, we'll keep with the speed theme and create a program that takes the distance traveled by an object and the time it took as inputs, and it returns the velocity:

```haskell
main :: IO ()
main = do
  putStrLn "provide the distance traveled by the object:"
  d <- getLine
  putStrLn "provide the time it took:"
  t <- getLine
  case read d `safeDiv` read t of
    Just v -> putStrLn $ "The velocity is: " ++ show v
    Nothing -> do
      putStrLn "\nThe time can't be zero!"
      main
```

By using this technique, we completely avoided a runtime error. It doesn't matter which numbers the user chooses. Our program won't crash! 🙌 But wait...  What if the user doesn't write valid numbers as inputs? What if the user writes letters or symbols instead of numbers? 😵

In that case, the `read` function won't be able to parse the `String` to a number, and... you guessed it... our program halts. 🫣

Worry not! We have a solution! But before that, I'm going to extract some functions so the code fits in the slides:

```haskell
vel :: Int -> Int -> Maybe Int
vel dist time = dist `safeDiv` time

getData :: IO (String, String)
getData = do
  putStrLn "provide the distance traveled by the object:"
  d <- getLine
  putStrLn "provide the time it took:"
  t <- getLine
  return (d, t)
```

Extracting the `velocity` function isn't of much help now, but it will be in the future. And extracting the `getData` action hides code we don't care about. So our `main` function looks like this now:

```haskell
main :: IO ()
main = do
  (d, t) <- getData
  case vel (read d) (read t) of
    Just v -> putStrLn $ "The velocity: " ++ show v
    Nothing -> do
      putStrLn "\nThe time can't be zero!"
      main

```

Much cleaner! Ok, so we would like a `read` function that can avoid returning a meaningful value if it's unable to parse the value. Luckily for us, a function like that already comes with our base libraries, and it's inside the `Text.Read` module:


```haskell
import Text.Read (readMaybe)

-- readMaybe :: Read a => String -> Maybe a

readMaybe "57" :: Maybe Integer
readMaybe "B00!" :: Maybe Integer
```


    Just 57



    Nothing


By using `readMaybe` instead of `read`, we avoid **all runtime errors due to a bad user input**:

```haskell
import Text.Read (readMaybe)
 
main :: IO ()
main = do
  (d, t) <- getData
  case vel (readMaybe d) (readMaybe t) of
    Just v -> putStrLn $ "The velocity is: " ++ show v
    Nothing -> do
      putStrLn "\nPlease, provide valid numbers (time /= zero)!"
      main
```

We do have a small issue now. `velocity` cannot take `Int`s directly anymore because `readMaybe` returns a `Maybe`. So we have to modify `velocity` like this:

```haskell
vel :: Maybe Int -> Maybe Int -> Maybe Int
vel (Just dist) (Just time) = dist `safeDiv` time
vel  _           _          = Nothing
```

We pattern match for the cases when both are parsed correctly and pass the `Int`s to the `safeDiv` function. We ignore all other cases and return `Nothing`.

There are many pre-defined functions that use the `Maybe` type, both in the standard libraries and in libraries made by other developers. When you encounter them, you can use them all in the same way. Pattern-match, handle both cases, and you're good to go!

### Benefits of optional values

Now that we understand how to use the `Maybe` data type, let's talk about the benefits it provides:

- You can handle the absence of value

`Maybe` provides a type-safe way to indicate the absence of a value, preventing many forms of runtime errors.

- Your code is more robust

Using `Maybe` forces you to explicitly handle both the case when you have a value and when you don't. Ensuring you address all possibilities and write more robust code.

- Allows you to express uncertainty

Optional values allow the developer to express uncertainty in a way that the consumer of the function is aware of and can handle without the need to know how the function is implemented.

- You can compose optional values

As we did in the previous examples. We provided the output of `readMaybe` as inputs to `velocity` that used `safeDiv` inside. If something went wrong at any time, the `Nothing` value would propagate throughout the function tree.

Now, the `Maybe` data type is not the only data type in town we can use to handle possibly problematic values. One downside of using the `Maybe` data type is that when we have several layers of optional values, we don't know what went wrong! In the previous code, for example. If we got a `Nothing` at the end, what failed? The parsing of the first value? The parsing of the second value? Or the division by zero? We have no way to find out!

Enter the `Either` data type:

## Ok, you `Either` give me a value or a reason why you didn't!

The `Either` data type is also pretty simple:

```haskell
data Either a b = Left a | Right b
```

The `Either` type represents values with two possibilities: a value of type `Either a b` is either `Left a` or `Right b`. You cannot have both. This is not necessarily related to error handling or optional values. Either can be used in many scenarios to represent a binary choice.

However, in the context of error handling, you can think of `Either` as something that builds on top of the `Maybe` data type to provide a way to log what went wrong. 

The way we use `Either` to handle errors is not forced in code but by convention. The community overall uses the `Left` constructor to hold an error value and the `Right` constructor to hold a correct value. Based on the mnemonic that "right" also means "correct."

So, now that we know about `Either`, let's modify `saveDiv` to let us know if we try to divide by zero:


```haskell
safeDivE :: Integral a => a -> a -> Either String a
safeDivE _ 0 = Left "You can't divide by zero, you fool!!"
safeDivE x y = Right (x `div` y)


15 `safeDivE` 3
15 `safeDivE` 0
```


    Right 5



    Left "You can't divide by zero, you fool!!"


As you can see, we chose `String` as the type parameter for `Left` so we can leave a message to the user or developer. Although using a simple `String` to leave a message is the intuitive thing to try first, using values that we could later handle programmatically is even better. Something like:

```haskell
data PermissionLevel = Guest | User | Admin

data UIException = WrongInput String
                 | WrongPermission PermissionLevel
                 | UserDidNotLogIn

someFunction :: Integral a => a -> a -> Either UIException a
```

And further down the line, we can use this extra information to do things programmatically. For example, if `Either` returns `Left UserDidNotLogIn`, we can redirect the user to the login page.

We will stick to `String`s for today's examples. To keep it simple.

There are also many functions already available that return `Either` values. As you might imagine, one of those is the `Either` equivalent of the `readMaybe` function:


```haskell
import Text.Read (readEither)

--readEither :: Read a => String -> Either String a

readEither "57" :: Either String Int
readEither "B00!" :: Either String Int
```


    Right 57



    Left "Prelude.read: no parse"


Notice how it returns what we otherwise would get as an Exception, but this time we're getting it as a plain `String`, and our program keeps running.

With all that, let's transform the rest of the code. The main function is almost the same, we just change `readMaybe` for `readEither`. And because we can return a custom messge with the error, we print that:

```haskell
import Text.Read (readEither)

main :: IO ()
main = do
  (d, t) <- getData
  case vel (readEither d) (readEither t) of
    Right v -> putStrLn $ "The object's velocity is: " ++ show v
    Left s -> do
      putStrLn s
      main
```

The `velocity` function is the one that gets interesting. Now, we can return a message, so we can personalize it based on what went wrong:

```haskell
vel :: Either String Int -> Either String Int -> Either String Int
vel (Right d) (Right t) = d `safeDivE` t
vel (Left  d) (Left  t) = Left $ "Both wrong!! d:"++ d ++" t:"++ t
vel (Left  d) _         = Left $ "Wrong distance input!: " ++ d
vel _         (Left  t) = Left $ "Wrong time input!: " ++ t
```

As you can see, the `velocity` function now explicitly handles more cases because we care about which input failed to parse. And that's pretty much it!

## From `Exception`s to optional values

Finally, there are ways to go from one to the other. We already saw how to go from normal values to exceptions... We throw an exception.

But what if we have an exception and we want an optional value? The easiest way to do that is using the `try` function:

```haskell
try :: Exception e => IO a -> IO (Either e a)
try ioa = (ioa >>= return . Right) `catch` (return . Left)
```

The `try` function uses `catch` under the hood. `try` takes an IO action and tries to run it. If everything goes well, it returns the final result using the `Right` constructor. But, if something goes wrong, `catch` will capture all exceptions of type `e` and return them using the `Left` constructor. Same as before, if the raised exception is of a type different than `e`, it will keep propagating.

So, let's say we want to read a file:

```haskell
main :: IO ()
main = do
  contents <- putStrLn "Where's the file?" >> getLine >>= readFile
  putStrLn $ "result: " ++ contents

```

In this case, we cannot prevent the possibility of an `Exception`. We're interacting with the outside world, and unexpected things might happen.

But! Using the `try` function, we can handle the `Exception` under the hood and only interact with optional values ourselves. Like this:

```haskell
main :: IO ()
main = do
  ec <- try $ putStrLn "Where's the file?" >> getLine >>= readFile
  case ec of
    Left e -> do
      putStrLn $ "WARNING: " ++ show (e :: IOException)
      main
    Right contents -> putStrLn $ "result: " ++ contents

```

As you can see, the exception, in this case, is of type `IOException`. These are exceptions related to IO operations, like reading and writing files. And from now on, you can do whatever you want with your values following the usual execution of your program.

OK. We learned how to handle exceptions using the exception mechanism, how to use optional values to keep our error handling within the regular execution of our program, and finally, how to move from one to the other. To end this lecture, let's compare the two of them to see what the tradeoffs are.

## Tradeoffs

In theory, the key difference between using exceptions and optional values is that when we use optional values, we're handling errors in the program itself. We're not stopping the execution and silently propagating the error throughout the stack. The error handling happens in a specific place and is part of the normal execution of the program.

In practice, there are more things to take into account.

|Optional values | Exceptions |
| --- | --- |
<span style="color:green;">They are evident by the types, so you know when you're dealing with them</span> | <span style="color:red;">They are hidden from you, so you're unaware of how a function can fail by only looking at its type</span>|
| <span style="color:green;">They are pure values, easy to predict</span> | <span style="color:red;">You can only catch them inside impure code, which makes your program harder to predict</span>|
| <span style="color:green;">They are easy to use</span> | <span style="color:red;">They can be complicated to use</span> |
| <span style="color:black;">Managing multiple levels of optional values <i>may*</i> add complexity to the code</span> | <span style="color:black;">The code complexity of the "happy path" stays the same, but there's added complexity elsewhere.</span> |
| <span style="color:red;">They don't compose well with functions that take the unwrapped value. Which generates a loss in modularity</span> | <span style="color:green;">They don't affect composability</span> |
| <span style="color:red;">Accidental strictness: It's not unavoidable, but it becomes easy to make our functions strict</span> | <span style="color:green;">Doesn't affect laziness and compiler optimizations</span> |
| <span style="color:red;">They can't handle asynchronous `Exceptions` cause those happen outside the code</span> | <span style="color:green;">We can `catch` asynchronous `Exceptions`</span> |

### So, what should I use?

For now:

- Try to use optional values (Maybe, Either, etc.) as much as possible to implement pure algorithms.

- If a failure is expected, it's better to explicitly indicate it with an optional type. For example, the `lookup :: (Eq a) => a -> [(a,b)] -> Maybe b` function returns a `Maybe` because it's expected that the key we asked for might not be there. 

- If the failure is a rare "This should never happen" case, using an `Exception` is usually the best choice.

- In many cases, the choice between exceptional and non-exceptional can be blurry. In those cases, you'll have to use your best judgment.

And finally, if you feel like this is not enough, don't worry. Further along the line, you'll encounter knowledge that will unlock new ways of handling errors and exceptions. Giving you even more flexibility!

# That's it for today! 😄
