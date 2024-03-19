# Modules

## Outline

* Importing Modules
    * Controlling environments
    * Controlling namespaces
* Creating our own Modules
* The Prelude and Standard Libraries

<div class="alert alert-block alert-danger">
    <b>
    The video and written lessons differ because the video format allows for clear explanations through refactoring code, and the written one is more suitable for sequential explanations. 
    </b> 
    Use this to your advantage. If something doesn't make sense in one medium, maybe it will in the other!
</div>

Crudely speaking, a Haskell module is just a collection of related functions, types, and type classes that can be imported and used in your code. But they are more than just that.

**Modules allow us to structure, reuse, and maintain our code and environment.**

But before learning how to create our own modules, let's see how we can use pre-defined ones.

## Importing Modules

<div class="alert alert-block alert-warning">
We're going to import many modules several times during this lesson. So, if you run the cells sequentially, you will encounter errors when you shouldn't. In those cases, restart the Kernel (In the Kernel menu above) to get rid of all the imports and run only the cell you're on, skipping all the previous ones.
</div>

Let's say your app needs to manipulate files and folders. We can use a module called `System.Directory` that has a bunch of functions, actions, and types related to file and directory manipulation.

To import this module, we use the `import` keyword followed by the name of the module:

```haskell
import System.Directory
```

This must be done before defining any functions, so imports are usually done at the top of the file. By adding this line of code, we gain access to all the functions, actions, types, and typeclasses of the `System.Directory` module. You can check everything that comes with this module here (link).

One of the functions provided is `listDirectory`:

```haskell
listDirectory :: FilePath -> IO [FilePath]
```

It takes a directory path of type `FilePath` (which is just a type synonym for String) and returns an IO action that, when performed, returns a list of all entries (files and directories, everything) inside the directory we passed as parameter.

So, if we use it to check what's inside the current directory of this JupyterNotebook, we get:


```haskell
import System.Directory

listDirectory "."
```


    ["23-State-Monad.ipynb","21-Reader-Monad.ipynb","24-Monadic-functions.ipynb","09-Creating-parameterized-and-recursive-types.ipynb","04-Pattern-matching.ipynb","jupyter-tutuorial.ipynb","ourProgram.o","rise.css","simpleProgram.hs","06-Recursion-and-folds.ipynb","22-Writer-Monad.ipynb","simpleProgram","ourProgram.hi","13-Bits-Bytes.ipynb","10-Creating-Type-Classes.ipynb","15-Learning-on-your-own-and-project.ipynb","16-Semigroup-and-Monoid.ipynb","03-Conditions-and-helper-constructions.ipynb","14-Handling-Errors.ipynb","19-Aeson.ipynb","hello.hi","02-Functions-Data-Types-and-Signatures.ipynb","ourSourceCode.hi","hello.o","17-Functor.ipynb","11-Basic-IO.ipynb",".ipynb_checkpoints","20-Monad.ipynb","12-Pragmas-Modules-and-Cabal.ipynb","25-Monad-Transformers.ipynb","18-Applicative.ipynb","ourSourceCode.o","01-Introduction-to-haskell.ipynb","08-Creating-non-parameterized-types.ipynb","05-Improving-and-combining-functions.ipynb","07-Intro-to-Type-Classes.ipynb"]


As you can see, the current folder contains the files of all the lessons we covered so far.

Now, let's say we want to write a function to find files inside the current directory that contain a certain String as part of their name. Something like this:

```haskell
import System.Directory

find' :: String -> IO [FilePath]
find' str = do
  entry <- listDirectory "."
  let found = -- filter entries
  return found
```

First, we get a list of all the files and directories using listDirectory, and then we filter them. 

We could easily create the filtering function ourselves with some pattern matching and recursion. But realistically, this sounds like a common-enough function to be available as a library. And it is!!

There's also a module called [`Data.List`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List.html) that contains dozens of functions to work with lists.

One of them is called `isInfixOf`. It takes two lists and returns `True` if the first list is contained, wholly and intact, anywhere within the second. Exactly what we need, so let's use it:


```haskell
import System.Directory
import Data.List

find' :: String -> IO [FilePath]
find' str = do
  entry <- listDirectory "."
  let found = filter (str `isInfixOf`) entry
  return found

find' "11"
```


    ["11-Basic-IO.ipynb"]


Awesome! Because we have access to modules with pre-written code, we don't have to implement everything by ourselves!

Ok, so the function looks good, but it has a weird name. Why not just call it `find` without the `'`?

Now JupyterNotebook is, again, doing some magic to avoid errors. But if we tried to change the name of the function to `find` without the `'` and compile this code as a regular Haskell program (which you will be able to do after this lesson), we'd get this error:

```
Ambiguous occurrence ‘find’
    It could refer to
       either ‘Data.List.find’,
              imported from ‘Data.List’ at YourFileName.hs:3:1-16
              (and originally defined in ‘Data.Foldable’)
           or ‘YourFileName.find’, defined at YourFileName.hs:13:1
```

The error is clear. We have two functions with the same name. One in our file and one that came when we imported `Data.List`. So the compiler doesn't know which one we're referring to.

There are multiple solutions to this. 

### Controlling the environment

The easiest solution, of course, is to change the name of our function:


```haskell
import System.Directory
import Data.List

findEntry :: String -> IO [FilePath]
findEntry str = do
  entry <- listDirectory "."
  let found = filter (str `isInfixOf`) entry
  return found

findEntry "11"
```


    ["11-Basic-IO.ipynb"]


But this doesn't solve the underlying issue that our environment is polluted with dozens of functions and types of both `System.Directory` and `Data.List` that we're not planning to use. Which can cause all sorts of troubles.

A better solution is to import a specific function or type instead of the whole module. We can easily do it like this:


```haskell
import System.Directory (listDirectory) -- import lsitDirectory from System.Directory
import Data.List (isInfixOf)            -- import isInfixOf     from Data.List

find :: String -> IO [FilePath]
find str = do
  entry <- listDirectory "."
  let found = filter (str `isInfixOf`) entry
  return found

find "11"
```


    ["11-Basic-IO.ipynb"]


We add the functions inside a parenthesis on the right. And if we need to import more than one, we add it separated by a comma.

For example, if we want to sort the entries before returning them, we can import the `sort` function from `Data.List` and use it like this:


```haskell
import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)      -- import isInfixOf and sort from Data.List

find :: String -> IO [FilePath]
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry -- filter and sort
  return found

find "Creating"
```


    ["08-Creating-non-parameterized-types.ipynb","09-Creating-parameterized-and-recursive-types.ipynb","10-Creating-Type-Classes.ipynb"]


And we can keep adding the functions we need when we need them.

Finally, if we happen to need many functions of a module, importing them one by one might be cumbersome. Also, we might end up with a huge list of functions and types that contain almost the whole module.

For those cases, you can use the `hidden` keyword. For example, let's say our `find` function is just one of many in our file. And we need to use a lot of functions provided by `Data.List`.

We can change the import like this:


```haskell
import System.Directory (listDirectory)
import Data.List hiding (find)  -- import everything from Data.List, except `find`

find :: String -> IO [FilePath]
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  return found

find "Creating"
```


    ["08-Creating-non-parameterized-types.ipynb","09-Creating-parameterized-and-recursive-types.ipynb","10-Creating-Type-Classes.ipynb"]


Here, we're importing everything from `Data.List` except for the `find` function.

And, same as before, you can hide more functions and types by adding them inside those parentheses separated by commas.

And that's how you can control your environment while importing modules.

But there's one more case we don't have a solution for. What if we import two modules that have functions with the same name?

For example:


```haskell
import System.Directory (listDirectory)
import Data.List hiding (find)
import Data.Map

find :: String -> IO [FilePath]
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  return found

find "Creating"

-- 👇 More code that uses `filter` from Data.Map
-- ...
```


    <interactive>:4:22: error:
        Ambiguous occurrence ‘filter’
        It could refer to
           either ‘Data.Map.filter’, imported from ‘Data.Map’ (and originally defined in ‘Data.Map.Internal’)
               or ‘Prelude.filter’, imported from ‘Prelude’ (and originally defined in ‘GHC.List’)


Let's say we import a module called `Data.Map`. This module has types and functions that allow you to store associations between unique keys and values. For now, don't worry about it. We'll take a proper look at this module in future lessons.

What's interesting is that both `Data.Map` and `Prelude` (a module we'll learn about shortly) provide a function called `filter`. The same as before, we have two different functions with the same name, and Haskell doesn't know which one we're referring to.

In this scenario, we don't want to hide Map's `filter` function because we're using it somewhere else in our code. And we cannot change their name because we're importing both from other modules.

Now is when namespaces come in handy!

### Controlling namespaces

Without all the jargon: 

**A namespace is an environment created to hold a group of names.**

We've been talking about "our environment" as a single thing. A "space" that contains our functions, types, and type classes all mixed together. But thanks to modules, we can have different environments (or namespaces).

And is as easy as adding a single word to the import:


```haskell
import qualified System.Directory (listDirectory) -- qualified import

System.Directory.listDirectory "." -- This works
listDirectory "." -- This doesn't work now
```


    ["23-State-Monad.ipynb","21-Reader-Monad.ipynb","24-Monadic-functions.ipynb","09-Creating-parameterized-and-recursive-types.ipynb","04-Pattern-matching.ipynb","jupyter-tutuorial.ipynb","ourProgram.o","rise.css","simpleProgram.hs","06-Recursion-and-folds.ipynb","22-Writer-Monad.ipynb","simpleProgram","ourProgram.hi","13-Bits-Bytes.ipynb","10-Creating-Type-Classes.ipynb","15-Learning-on-your-own-and-project.ipynb","16-Semigroup-and-Monoid.ipynb","03-Conditions-and-helper-constructions.ipynb","14-Handling-Errors.ipynb","19-Aeson.ipynb","hello.hi","02-Functions-Data-Types-and-Signatures.ipynb","ourSourceCode.hi","hello.o","17-Functor.ipynb","11-Basic-IO.ipynb",".ipynb_checkpoints","20-Monad.ipynb","12-Pragmas-Modules-and-Cabal.ipynb","25-Monad-Transformers.ipynb","18-Applicative.ipynb","ourSourceCode.o","01-Introduction-to-haskell.ipynb","08-Creating-non-parameterized-types.ipynb","05-Improving-and-combining-functions.ipynb","07-Intro-to-Type-Classes.ipynb"]



    <interactive>:1:1: error:
        • Variable not in scope: listDirectory :: String -> t
        • Perhaps you meant ‘IHaskellDirectory.listDirectory’ (imported from System.Directory)


<div class="alert alert-block alert-warning">
If you run the previous cell, <code>listDirectory "."</code> will work because Jupyter's environment already has <code>System.Directory</code> imported without qualifying it. If you want to reproduce this error, you'll have to restart the kernel and run the above cell first.
</div>

By adding the `qualified` keyword after `import`, instead of adding the `listDirectory` to our environment, we create a new one called `System.Directory` that contains it.

That way, each time we want to use `listDirectory`, we have to look for it inside the `System.Directory` namespace.

This solves our previous problem:


```haskell
import System.Directory (listDirectory)
import Data.List hiding (find)
import qualified Data.Map

find :: String -> IO [FilePath]
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  return found

find "Creating"

-- 👇 More code that uses `Data.Map.filter` from Data.Map
-- ...
```


    ["08-Creating-non-parameterized-types.ipynb","09-Creating-parameterized-and-recursive-types.ipynb","10-Creating-Type-Classes.ipynb"]


As you can see, we don't have errors no more. Now that we `qualified` the `Data.Map` import, we're still importing everything from that module, including the `filter` function. But all that is inside the `Data.Map` namespace, so it doesn't get mixed with the functions/types/type classes in our main environment.

And, to show how we would use code from `Data.Map`, let's take that list of filtered and ordered entries and transform them into a map:


```haskell
import Data.List hiding (find)
import System.Directory (listDirectory)
import qualified Data.Map

find :: String -> IO (Data.Map.Map Int String)
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  let foundMap = Data.Map.fromList $ zip ([1 ..] :: [Int]) found -- List to Map
  return foundMap

find "Creating"
```


    fromList [(1,"08-Creating-non-parameterized-types.ipynb"),(2,"09-Creating-parameterized-and-recursive-types.ipynb"),(3,"10-Creating-Type-Classes.ipynb")]


We only added a single line of code. As we said before, maps store associations between unique keys and values. We have the values but without keys!

We'll use the `zip` function to assign a unique key to each value. As we saw on the recursion lesson's homework, the `zip` function takes two lists and returns a list of tuples with the corresponding pairs.

We're zipping an infinite list of ordered numbers starting from one and the list of filtered and sorted entries. So, we should get a list of pairs with a number as the first element and an entry as the second.

Conveniently enough, the `Data.Map` module provides a function called `fromList` that takes a list of pairs and returns a value of type `Map`. In this case, the value it returns is of type `Map Int String` because the keys are `Int` and the values are `String`.

With this final feature, we've gained complete control of our environments. Although, writing `Data.Map` everywhere gets old pretty quickly. And if we qualify imports with longer names or qualify several modules, our code starts to get cluttered and harder to read, like this sentence.

Haskell allows us to rename the namespace to a more convenient one. For example:


```haskell
import Data.List hiding (find)
import System.Directory (listDirectory)
import qualified Data.Map as Map -- Renamed namespace

find :: String -> IO (Map.Map Int String)
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  let foundMap = Map.fromList $ zip ([1 ..] :: [Int]) found -- List to Map
  return foundMap

find "Creating"
```


    fromList [(1,"08-Creating-non-parameterized-types.ipynb"),(2,"09-Creating-parameterized-and-recursive-types.ipynb"),(3,"10-Creating-Type-Classes.ipynb")]


<div class="alert alert-block alert-warning">
    Notice that module names are capitalized, if you are renaming them, this new name has to be capitalized as well!
</div>

And as a final tip, we can combine all these different techniques. For example, if two modules do kind of the same thing and don't have any name clashes, we could give both namespaces the same name and treat them as if they came from a single module.

This doesn't apply right now, but there's an import combination that does. Our `find` function looks pretty good. But one thing that bothers me is that `Map.Map`. I don't mind the `Map.fromList`. Actually, I prefer it! That lets me know that `fromList` comes from the `Data.Map` module. But `Map.Map` is kind of redundant. Of course that the `Map` type constructor comes from the `Data.Map` module!

Let's combine a couple of imports to avoid this redundancy!:


```haskell
import Data.List hiding (find)       
import System.Directory (listDirectory)
import qualified Data.Map as Map hiding (Map) -- import qualified + Rename namespace + hide Map
import Data.Map (Map)                         -- Import only Map

find :: String -> IO (Map Int String)
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  let foundMap = Map.fromList $ zip ([1 ..] :: [Int]) found
  return foundMap
  
find "Creating"
```


    fromList [(1,"08-Creating-non-parameterized-types.ipynb"),(2,"09-Creating-parameterized-and-recursive-types.ipynb"),(3,"10-Creating-Type-Classes.ipynb")]


By hiding the `Map` type constructor from the qualified import and importing it separately, we essentially removed the `Map` type constructor from the `Map` namespace and added it to our main namespace.

Everything else is the same, but now, the signature of `find` is easier to read.

That's pretty much everything about importing modules and managing your environment. But remember, we said modules also allow us to better structure, reuse, and maintain our code? Well, let's see how!

## Creating your own module

Since modules are just plain Haskell files that can be imported into other Haskell files, it is easy to create a module on your own.

Let's say we want another version of the `sum` function that returns an error if we apply it to the empty list instead of the value 0 that `sum` returns.

To create a module that exposes such a module, we first create a Haskell file that we call `SumNonEmpty.hs`. At the top of this file, we write a module statement like
```haskell
module SumNonEmpty where
```

With this statement, we defined the name of our module as `SumNonEmpty`, which again should start with an upper case letter.

**It is good practice to have the module's name as the name of the file**, though this is not mandatory.

And now, we can write the code that our module provides:


```haskell
module SumNonEmpty where

data MyData a b = Error a | Result b deriving (Show)

sumNonEmpty :: Num a => [a] -> MyData String a
sumNonEmpty [] = Error "List is empty"
sumNonEmpty xs = Result (sum xs)
```

And that's pretty much it! We created our own module. 

Now we can import it in another file (in the same folder) like any other module:


```haskell
import SumNonEmpty

sum []       -- 0
sum [1..3]   -- 6

sumNonEmpty []     -- Error "List is empty" 
sumNonEmpty [1..3] -- Result 6
```


    0



    6



    Error "List is empty"



    Result 6


In the previous example, the exported module is in the same folder as the file that imports it. But they could be in different places. In that case, the imports themselves express where the code is located.

For example, in this case:

```haskell
import Data.Time.Calendar
import Data.Time.Clock.System
```

We can infer that the imports translate to the next file structure:

```
Data
  | 
  |--- Time
         |
         |--- Calendar.hs 
         |--- Clock
                 | 
                 |--- System.hs
                 
```

Where the `Calendar` module is inside the `Data/Time/Calendar.hs` file, and the `System` module is inside the `Data/Time/Clock/System.hs` file.

In contrast to import restrictions, as we did until now, Haskell also gives you control over exports. That is, what the module exposes to the outside world.

In the above example, our module exports all that is declared in its file. But there are plenty of cases when you don't want that. For example, when you create a helper function that is meant to be used only inside the module, like this:


```haskell
module SumNonEmpty1 where

errorMessage1 = "List is empty"

data MyData1 a b = Error1 a | Result1 b deriving (Show)

sumNonEmpty1 :: Num a => [a] -> MyData1 String a
sumNonEmpty1 [] = Error1 errorMessage1
sumNonEmpty1 xs = Result1 (sum xs)
```

In this case, anyone that imports `SumNonEmpty1` has access to `errorMessage`. But it doesn't make sense to use this error message outside the `sumNonEmpty` definition. So there's no reason for the consumer of the module to be able to access it!

The solution is simple: Explicitly instantiate what the module exports. Like this:


```haskell
module SumNonEmpty2 (sumNonEmpty2, MyData2) where

errorMessage2 = "List is empty"

data MyData2 a b = Error2 a | Result2 b deriving (Show)

sumNonEmpty2 :: Num a => [a] -> MyData2 String a
sumNonEmpty2 [] = Error2 errorMessage2
sumNonEmpty2 xs = Result2 (sum xs)
```

<div class="alert alert-block alert-info">
   Note: It is common practice to put one export per line if they don't fit in one line.
</div>

By explicitly stating that the module exports `sumNonEmpty, MyData`, everything else inside the module becomes inaccessible to the end user (the one that imports it). As you can see here:


```haskell
import SumNonEmpty2

sumNonEmpty2 [3.5, 7]

errorMessage2
```


    Result2 10.5



    <interactive>:1:1: error:
        • Variable not in scope: errorMessage2
        • Perhaps you meant ‘errorMessage1’ (imported from SumNonEmpty1)


But we also created a problem now. We exported the type `MyData2` but not its constructors. That means that we have no way of extracting the result by pattern-matching:


```haskell
import SumNonEmpty2

resOfSum = sumNonEmpty2 [100..150]

resOfSum

twiceAsMuch Error2 _ = 0
twiceAsMuch Result x = x * 2
```


    Result2 6375



    <interactive>:1:13: error:
        Not in scope: data constructor ‘Error2’
        Perhaps you meant one of these: ‘Error’ (imported from SumNonEmpty), ‘Error1’ (imported from SumNonEmpty1), variable ‘error’ (imported from Prelude)


This is usually solved in two different ways. Either by exporting the constructors so we can use them when importing the module:

```haskell
-- Alternative one:
module SumNonEmpty2 (sumNonEmpty2, MyData2 (Error2, Result2)) where
-- Alternative two:
module SumNonEmpty2 (sumNonEmpty2, MyData2 (..)) where -- (..) means "all constructors".
```

Or by keeping the constructors unaccessible but exporting a function that can extract them:


```haskell
module SumNonEmpty3 (sumNonEmpty3, MyData3, getResult) where

errorMessage3 = "List is empty"

data MyData3 a b = Error3 a | Result3 b deriving (Show)

sumNonEmpty3 :: Num a => [a] -> MyData3 String a
sumNonEmpty3 [] = Error3 errorMessage3
sumNonEmpty3 xs = Result3 (sum xs)

getResult :: (Num a) => a -> MyData3 String a -> a
getResult def (Result3 x) = x
getResult def _           = def
```


```haskell
import SumNonEmpty3

resOfSum = sumNonEmpty3 [100..150]

resOfSum

getResult 99  resOfSum
getResult 99 (sumNonEmpty3 [])
```


    Result3 6375



    6375



    99


Now that we know how to work with modules, let's learn about the most famous of all.

### The `Prelude` module

When working in GHCi or writing your own Haskell code, some functions are available by default, for instance, `head`, `sum`, and `length`.

This is because those functions are part of a module called **Prelude** that is imported by default. 

The word prelude means an introduction to something more important. Which is the code and modules you will write.

So, the `Prelude` module provides basic functions, data types, and type classes that the average developer might need for its code.

You can find a list of all the functions, types, and type classes provided by **Prelude** [here](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html). 

Besides the Prelude, there are other standard libraries that aren't imported by default but are available to be imported without the need for you to download them separately.

### The Standard Libraries

If you go to [this website](https://downloads.haskell.org/ghc/latest/docs/libraries/), you can see all the libraries that come baked into Haskell.

It's daunting, I know. But you don't have to memorize all this. There are tools like [Hoogle](https://hoogle.haskell.org) that let you search through libraries whenever you need to. We'll explore how to use this and other convenient tools in a future lesson. For now, that's it for today!

## That's it for today!

This lesson's homework will be to read through everything provided by the Prelude.

But! In there, there's also plenty of stuff we didn't cover, and that depends on concepts we'll learn further down this course. So it's not required to understand everything. The objective is to have an idea of what the Prelude module provides, use it as a refresher of all the functions, types, and type classes we've used so far, and get you used to reading the official documentation.
