# Installing Haskell and creating our first program

We're starting to get used to coding in Haskell, and we're ready to get serious. In the next series of lessons, we'll learn how to manage our development environment, create Haskell Projects, deal with errors, and solve problems in general. Basic skills every Haskell developer must have.

This is the first lesson in the series. And it's a short one. Where we're going to set up our local development environment and compile our first program.

## Outline

* Installing Haskell
  - Installing GHCup
  - Installing GHC, Cabal, Stack, and HLS with GHCup
  - Installing VSCode Extensions
* Creating our first program
  - Writing the simplest Haskell program
  - Compiling and running our program

## Installing Haskell Tooling (On all operating systems)

Feel free to ignore this section if you don't want to install anything locally and want to keep using the online dev environment.

We need a way to transform our Haskell source code into native code that our computer is able to run. And for that, we need a compiler. The most widely used Haskell compier is GHC. Let's learn how to install and use it.

GHCi (the interactive environment) already comes with GHC. There are a few different ways to install GHC. We could download it directly from its website. But there are better options. For example:

- The [Stack](https://docs.haskellstack.org/en/stable/ ) tool.

- The [GHCup](https://www.haskell.org/ghcup/#) comand line tool

We'll use GHCup because Stack does more than just installing the Haskell tooling and I want to go step by step. But feel free to use Stack if you prefer it.

So, to install our tools, go to the [GHCup website](https://www.haskell.org/ghcup/#) and run the command it shows you on the terminal.

You can click on "Show all platforms" if your OS is not shown.

Once you run the command, it will ask you a few questions about what do you want to install. Make sure to have installed—at least—GHC, Haskell Language Server, and cabal-install.

And that's it! We have everything we need! Asuming, of course, that you have a text editor. And Mirosoft Word doesn't count.

If you don't have one, install [VSCode](https://code.visualstudio.com/). It's the most widely used and very friendly.

If VSCode ofers to install extensions, say yes. Else, search for "Haskell" in the extensions pan and isntall the two most downloaded ones.

OK! Enough with the setup! Let's compile our first program!

### Compiling Haskell programs

In this section, we will show how you can compile simple Haskell files, later we will show how you can compile a more complex project using Cabal.

In the previous lesson, we saw one of the shortest Haskell programs you could write. This one:


```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

A simple program that prints "Hello World" on the standard output. Remember that all your programs have to have an action called main that functions as the entry point of your program.

OK! Let's compile this bad boy!

Because we're on this Jupyter Notebook, we'll have to use some command line tools to make it happen. But you can just write a file with the `.hs` extension and compile it using `ghc` (without the `:!` prepended) like I show at the end.

So. first, we'll save the main action in a file with this command:


```haskell
:!echo "main = putStrLn \"Hello World!\"" >> hello.hs
```


    


If we look for a haskell file, we see that it's there:


```haskell
:!ls | grep 'hello'
```


    hello.hs


And, if we check its contents, we find only the main action:


```haskell
:!cat hello.hs
```


    main = putStrLn "Hello World!"


Ok, let's compile it! 

To compile a file, the only thing we need to do is to pass the file path as an argument to the ghc command line tool:


```haskell
:!ghc hello.hs
```


    [1 of 1] Compiling Main             ( hello.hs, hello.o )
    Linking hello ...


Now, if we look for files named outSourcecode, we find three new files:


```haskell
:!ls | grep 'hello'
```


    hello
    hello.hi
    hello.hs
    hello.o


- `hello.hs` is the file we created with the source code.
- `hello.o` is an object file and `hello.hi` is an interface file that will link the object file to the libraries that come with GHC to produce an executable. We don't really care about those right now.
- The one that we care about is `hello` (called `hello.exe` if we compile it on a Windows machine). This one is the acutal executable. A binary that we can run like like any other program.

So, let's run it!:


```haskell
:! ./hello
```


    Hello World!


And boom! We compiled and run our own Haskell program! Congratulations!

Of course, because GHCi came with GHC, we can also load the `hello.hs` file to GHCI (`ghci`) and play arround with the main function.

<div class="alert alert-block alert-info">
    GHCi the Haskell REPL allows you to load a Haskell file with the <code>:l</code> command. There, it is not relevant whether the file has a main function or not. Once the file is loaded into GHCi you can call any of the functions or types defined in the file and test them if they work as you expected. If you load a <code>main.hs</code> file into GHCi that imports some user-defined modules, they will also be included as in the compilation process.
</div>

We've been coding in Haskell for a while now. But everything has been quite simple and short. That's why, if you've been doing your homework, we've been always writting our whole code in a single file.

But what if we're making a more complex application? Like a website, a game, or a blockchain? How many thousands of unreadable code lines would that single file have? The naive solution is to split it into a bunch of files. But that stil doesn't solve many of problems we would have. That's why we use modules.

## That's it for today!
