# Gaining your independence 💪

## Outline

* Small tips and tricks
    * REPL
    * Hackage
    * Hoogle
    * `undefined`
    * Type Holes
* Section's Final Project

## REPL

Remember that you always have the repl available to you. And If you enter the REPL using `cabal repl`, you can also import and explore modules you downloaded using Hackage. If you want to see how it works, see the example in the video version.

## Hackage


## https://hackage.haskell.org/

Hackage is the Haskell community's central package archive. At the time of writing this lesson, there are over 16 thousand Haskell packages available in Hackage.

We already saw how you can use it with Cabal to add libraries to your projects. But in the video lesson, we'll explore how to find and choose libraries and explore the documentation.

## Hoogle


## https://hoogle.haskell.org/

Hoogle allows you to search a commonly used subset of Haskell libraries by either function name or approximate type signature. 

This is useful in several scenarios, for example:

1. If you want to print a string to the console but forget the name of the function, searching for "String -> IO ()" will provide all functions with a signature that matches your intention. 
2. If you want to use a function but forget from which module it is, you can search for the function, and it will tell you where it is.
3. If you want to work with some concept, like natural numbers or web sockets, you can try searching those terms to see if any library, type, module, function, or type class roughly matches the name.

## `undefined`

If we look for `undefined` in Hoogle, we'll see that, in theory, it's just a fancy error. It's a value that, as soon as you evaluate it, halts your program. Of course, as we saw in the Handling Errors lesson, we don't like runtime errors! So, why I'm sharing it as a tip?

In practice, `undefined` is a great tool to keep the type checker assisting you while working on a half-baked code. Let's see how.

Let's say we want to create a function that reads a CSV file, transforms the content into JSON format, and writes a new file with the new contents. 

First, we create newtype wrappers for both CSV and JSON values. To avoid mixing CSV and JSON values by accident and allow the compiler to provide more specific hints. Even if the underlying value in both cases is just a String.

Then, because we don't want our program to crash if, for some reason, the reading and writing of files or the conversion fails, we'll catch the errors and return an Either:

```haskell
newtype CSV = CSV String 
newtype JSON = JSON String

csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile ...
```

Ok, now we can start implementing the function. Because we're just starting and we don't have a clear idea of how we want to implement this, we'll go step by step. I'll behave naively to showcase the usefulness of `undefined`. 

Let's start by printing a message to the console and reading the file:

```haskell
csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile = do
  putStrLn "Reading CSV file"
  rawCSV <- readFile  -- ❌ typecheck: Error here!
```

If we write this, we get this error:

```
The last statement in a 'do' block must be an expression
  rawCSV <- readFile
```

Which is not the error we would expect. Especially because there's an even more fundamental error that the type checker is not telling us about because it's stuck in this one.

We could be making huge mistakes like this one:

```haskell
csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile = do
  putStrLn "Reading CSV file"
  askldj56jklsdjf564lkjsdlf  -- Cat walked on the keyboard 🐈
  rawCSV <- readFile         -- ❌ typecheck (oblivious to the previous line): Error here!
```

And we wouldn't know because the type checker is stuck with the same error as before.

This is one of the cases when `undefined` is handy. If we add an `undefined` as the last statement of the `do` block, we're essentially telling the type checker: "trust me, bro, from here on, everything is fine. Don't sweat it."

So, the type checker, assuming everything is fine there, will continue to analyze the rest of our code and give us useful information.

In this case, if we do this:

```haskell
csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile = do
  putStrLn "Reading CSV file" -- ❌ typecheck: Wrong type!
  askldj56jklsdjf564lkjsdlf   -- ❌ typecheck: What's wrong with you?
  rawCSV <- readFile          -- ❌ typecheck: Where's the readFile's argument?
  undefined
```

We get a bunch of helpful errors that help us realize that there's a line with gibberish, that we didn't specify the argument name of the `csvFileToJsonFile` function, and that we didn't provide the argument to the `readFile` function. So, we fix them:

```haskell
csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile fp = do
  putStrLn "Reading CSV file"
  rawCSV <- readFile fp
  undefined
```

Now, we have no type errors, and we can be reasonably certain that everything up until `undefined` is ok.

Another use of `undefined` is to check the behaviors during development. For example, we still have to parse the contents to CSV, convert them into JSON, and then write the new file. And on top of that, we have to do error handling. 

Instead of writing the whole thing and checking that everything works at the end, we can check the values in intermediate steps by running the code with the `undefine` there. For example, we can check the content of the files by printing them:

```haskell
csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile fp = do
  putStrLn "Reading CSV file"
  rawCSV <- readFile fp
  print rawCSV
  undefined
```

Now, we open a REPL and run `csvFileToJsonFile "somefile.csv"` and we get the contents of the file printed at the console, and after that, we get an exception:

```
*** Exception: Prelude.undefined
```

Because `undefined` is just a fancy error, we'll get a runtime error if we run the program. Of course, by the time you're done with the code, there has to be no `undefined` left. `undefined` is just a tool for your convenience during a development session. You could get fired if you ship an `undefined` value to production.

But! We don't care at this point because we're mid-development, and we just want to check if everything we coded so far is fine.

Finally, one last good use case for `undefined` is to use it as an implementation TODO. For example, let's say we keep going with our function:

```haskell
csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile fp = do
  putStrLn "Reading CSV file"
  rawCSV <- readFile fp
  let csv = parseCSV rawCSV -- ❌ typecheck: Who is parseCSV? A friend of yours?
  undefined
```

At this point, we'd have an error because there's no `parseCSV` function. And there's no `parseCSV` function because we haven't implemented it yet.

One option would be to implement `parseCSV` right away. That would be fine. But what if, halfway through implementing it, you realize you need to implement another function? And another one. This specific case wouldn't be that complicated. But you can see how, in more complex cases, by the time you finish implementing all the internal functions, you lose track of what you had in mind for the original one.

So, if you have a rough idea of the overall structure of the original function, you can defer implementing the internal functions until you finish implementing the original function by creating the internal functions signatures and setting the actual implementation of it as undefined. Like this: 

```haskell
newtype CSV = CSV String deriving (Show)
newtype JSON = JSON String deriving (Show)

csvFileToJsonFile :: FilePath -> IO (Either String JSON)
csvFileToJsonFile fp = do
  putStrLn "Reading CSV file"
  rawCSV <- readFile fp
  let csv         = parseCSV rawCSV
      (JSON json) = csvToJson csv
  writeFile "newFile.json" json
  undefined

parseCSV :: String -> CSV
parseCSV = undefined

csvToJson :: CSV -> JSON
csvToJson = undefined
```

And now, each `undefined` is like a TODO item. The first indicates that you still have to add error handling. And the other two that you still have to implement those functions. You essentially split the work into three, and you can start tackling your TODOs one by one.

Now, let's move to the final tip of the lesson: Type holes!

## Type holes

Typed holes are a feature of GHC specifically designed to help you figure out what code to write when you're unsure.

It works like this:

Let's say you're implementing a function that parses a list of `String`s into valid emails:

```haskell
newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = undefined
```

This function verifies that the user input is a valid email by checking it contains the `@` sign. And then, it has to normalize them by converting all characters into lowercase.

Ok. So, let's start easy by just straight-up converting Strings into Emails without doing anything.

If we change the undefined to an underscore, we get a pretty interesting error:

```haskell
newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = _
```
-------------------------------------------------------------
```
• Found hole: _ :: [String] -> [Email]
• In an equation for ‘parseEmails’: parseEmails = _
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:99:1)
  Valid hole fits include
    parseEmails
    mempty
  Valid refinement hole fits include
    map _
    concatMap _
    (<$>) _
    fmap _
    ($) _
    const _
    pure _
    head _
    last _
    id _
```

This is what type holes bring to the table in Haskell. By putting an underscore in our unfinished code, we asked the type checker for hints about what could be done there. The type checker brings all the information it can:

- It tells us what's the type of the hole.
- Where the hole is.
- What are relevant bindings (in this case, the only relevant binding is the same function we're defining, but if we have a `where` clause or `let` bindings, those would show up as well).
- Then, it shows us which values in our scope perfectly fit the hole.
- Finally, it tells us which functions and constructors don't fit perfectly but could take us one step closer to the final answer.

In this case, we know we have to `map` over the list of `String`s, so we take the first "refinement hole" suggestion and write `map` with an underscore (a new type hole) in front:

```haskell
newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map _
```
---
```
• Found hole: _ :: String -> Email
• In the first argument of ‘map’, namely ‘_’
  In the expression: map _
  In an equation for ‘parseEmails’: parseEmails = map _
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:100:1)
  Valid hole fits include Email
  Valid refinement hole fits include
    ($) _
    const _
    pure _
    return _
    ($!) _
    (Map.!) _
    head _
    last _
    id _
```

Now, we get a new set of the same information but for our new hole. And, if we look at the "Valid hole fits", we see that the `Email` constructor is there!

If we wrap a string with the `Email` value constructor, we get a value of type `Email`, which is exactly what we set out to do!

So, we take the type hole suggestion and write the `Email` constructor after `map`:

```haskell
newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map Email
```

And voilá! Our function compiles.

But we're far from finished here. We said we wanted to filter emails that didn't contain the `@` sign, so let's do that. 

Of course, we want to filter the emails before constructing them, so we'll use function composition to add the `filter` function before the `map` function: 

```haskell
newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map Email . filter _
```
---
```
• Found hole: _ :: String -> Bool
• In the first argument of ‘filter’, namely ‘_’
  In the second argument of ‘(.)’, namely ‘filter _’
  In the expression: map Email . filter _
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:94:1)
  Valid hole fits include
    null
    read
  Valid refinement hole fits include
    (==) _
    (/=) _
    (>) _
    (<=) _
    (>=) _
    (<) _
    ($) _
    head _
    last _
    id _
    (Some refinement hole fits suppressed; use -fmax-refinement-hole-fits=N or -fno-max-refinement-hole-fits)
 ```

Ok. So, we need a predicate. But, this time, the typed hole has a little message at the bottom. This is because it has more suggestions than the maximum allowed by default. One thing we could do to get more hints is to disable this maximum allowed by writing a pragma with the flag indicated right there like this:

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map Email . filter _
```
---
```
• Found hole: _ :: String -> Bool
• In the first argument of ‘filter’, namely ‘_’
  In the second argument of ‘(.)’, namely ‘filter _’
  In the expression: map Email . filter _
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:94:1)
  Valid hole fits include
    null
    read
  Valid refinement hole fits include
    (==) _
    (/=) _
    (>) _
    (<=) _
    (>=) _
    (<) _
    ($) _
    notElem _
    elem _
    any _
    all _
    const _
    pure _
    return _
    ($!) _
    head _
    last _
    id _
 ```

Now, we have more options in the "refinement hole fits" sections. And, if we look at them, we're reminded that we could use `elem`. We know that `elem` is a predicate that returns true if the element is inside the list, which is what we needed. We substitute `_` with `elem _` and keep going:

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map Email . filter (elem _)
```
---
```
• Found hole: _ :: Char
• In the first argument of ‘elem’, namely ‘_’
  In the first argument of ‘filter’, namely ‘(elem _)’
  In the second argument of ‘(.)’, namely ‘filter (elem _)’
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:95:1)
  Valid hole fits include
    maxBound
    minBound
  Valid refinement hole fits include
    head _
    last _
    id _
    pred _
    succ _
    toEnum _
    read _
 ```

This case is pretty obvious. We need a character to check if it is part of the `String`, and we know which character that is:

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map Email . filter (elem '@')
```

We're done with the filtering! Now, let's normalize the emails. Because we have to normalize the strings before wrapping them with the `Email` constructor, we do the same as before and compose a type hole:

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map (Email . _) . filter (elem '@')
```
---
```
• Found hole: _ :: String -> String
• In the second argument of ‘(.)’, namely ‘_’
  In the first argument of ‘map’, namely ‘(Email . _)’
  In the first argument of ‘(.)’, namely ‘map (Email . _)’
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:98:1)
  Valid hole fits include
    show
    reverse
    cycle
    init
    tail
    id
    mempty
    fail
    read
  Valid refinement hole fits include
    (:) _
    (++) _
    max _
    min _
    map _
    concatMap _
    (<$>) _
    fmap _
    take _
    drop _
    ($) _
    takeWhile _
    dropWhile _
    const _
    filter _
    (<>) _
    mappend _
    pure _
    sequenceA _
    foldMap _
    return _
    (<*>) _
    (=<<) _
    (<*) _
    (<$) _
    sequence _
    ($!) _
    asTypeOf _
    scanl1 _
    scanr1 _
    showChar _
    showString _
    head _
    last _
    id _
    mconcat _
```

We get a huuuuuge list of options, but the one that clearly looks like the best option is to refine our hole with a `map`. We have a list of characters. So, maybe we can go through every character and return the lowercase version, one by one. So, we accept that suggestion and replace the `_` with `map _`:  

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map (Email . map _) . filter (elem '@')
```
---
```
• Found hole: _ :: Char -> Char
• In the first argument of ‘map’, namely ‘_’
  In the second argument of ‘(.)’, namely ‘map _’
  In the first argument of ‘map’, namely ‘(Email . map _)’
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:100:1)
  Valid hole fits include
    id
    pred
    succ
  Valid refinement hole fits include
    max _
    min _
    ($) _
    const _
    pure _
    return _
    ($!) _
    asTypeOf _
    head _
    last _
    id _
```

Ok. We know that we need a function that goes from character to character. But none of the provided ones seem to either fit perfectly or help us move in the right direction. But we have one more ace up our sleeve: Imports! 

We get those suggestions because those are the ones available in our environment. So, if we want more suggestions, we can add more to our environment. In this case, we want to work with characters, so a good initial idea would be to import a module full of functions to work with characters. The `Data.Char` module is the prime candidate. Let's do that and see which new options we get:

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

import Data.Char

newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map (Email . map _) . filter (elem '@')
```
---
```
• Found hole: _ :: Char -> Char
• In the first argument of ‘map’, namely ‘_’
  In the second argument of ‘(.)’, namely ‘map _’
  In the first argument of ‘map’, namely ‘(Email . map _)’
• Relevant bindings include
    parseEmails :: [String] -> [Email]
      (bound at /Users/roberm/scratchpad/typedHoles.hs:100:1)
  Valid hole fits include
    id
    pred
    succ
    toLower
    toUpper
    toTitle
  Valid refinement hole fits include
    max _
    min _
    ($) _
    const _
    pure _
    return _
    ($!) _
    asTypeOf _
    head _
    last _
    id _
```

Behold! Between the new suggestions, there's a function that perfectly fits our hole with the name of `toLower`. It looks too enticing to ignore, so let's replace it:

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

import Data.Char

newtype Email = Email String deriving Show

parseEmails :: [String] -> [Email]
parseEmails = map (Email . map toLower) . filter (elem '@')
```

It compiles! And it looks like we finished implementing all the functionality we wanted. And if we test the function:

```haskell
parseEmails ["luffy@onepiece.com","ZorO@OnePiece.cOm", "son.goku"]

-- [Email "luffy@onepiece.com",Email "zoro@onepiece.com"]
```

We see that it works as expected. 

As you can see, type holes can be really useful. Especially when you're working with many polymorphic values or nested structures. Just as a final remark, you can have more than one hole at a time and name them by adding the name right after the underscore (without a space in between). Make it easier to distinguish them.

## Section's Final Project (what we did so far)

In this lesson, you're going to prove to yourself that you can code in Haskell.

If you've been doing the homework by now, you have a couple of programs under your belt:

* In lesson 9's homework, you built a closed and open maze (we called it Forest) solver.  
* In lesson 11's homework, you had the opportunity to build a program that prints a tree-like graph representing a folder structure.
* In lesson 14, we built a CLI game in which the user tries to escape from a forest before it runs out of stamina. And in the homework of the same lesson, you added a battle system to fight golems while trying to escape.
* In lesson 15's homework, you went through and understood the code of a CLI program that you can use to manage your to-do list. And on top of that, you added error handling to fix the bugs I purposely hid in the code.

At every step of the way, we introduced new concepts, and I provided you with thorough guidance about what to take into account and how to approach those challenges. Now, it's time for you to build something by yourself.

Here are the requirements:

## Section's Final Project (project requirements)

* Build a Tic-Tac-Toe game.
* It has to be a CLI executable with the business logic implemented as a library.
* There has to be a single-player (against the machine) and a multiplayer (two-player) mode.
* The machine has to play randomly.
* The board has to be printed nicely on the console.
* Use any library you want. However, the provided solution will only use the `random` library and Haskell features explained up until now. So, if you're tempted to use more advanced features, you're likely overcomplicating it.

The only way to learn how to code is by coding. So, make sure to do the project, and I'll see you on the next one.
