# Cabal and Language Extensions

## Outline 

- Cabal
    - Introduction to Cabal
    - Creating a new Haskell project
    - Going over the Cabal file using an external library
    - Building and running our executable
- Language extensions and Pragmas
    - NumericUnderscores
    - TypeApplications

<div class="alert alert-block alert-danger">
    <b>
    The video and written lessons differ because the video format allows for explanations through exemplification, and the written one is more suitable for sequential explanations. 
    </b> 
    Use this to your advantage! 😃 If something doesn't make sense in one medium, maybe it will in the other!
</div>

## Cabal

[Cabal](https://cabal.readthedocs.io/en/stable/intro.html) is a system for building and packaging Haskell libraries and programs. The name **cabal** stands for *Common Architecture for Building Applications and Libraries*.

The term cabal is overloaded. It can refer to either: cabal-the-spec (.cabal files), cabal-the-library (code that understands .cabal files), or cabal-the-tool (the `cabal-install` package which provides the `cabal` executable);

Because these three usually work in tandem, we're going to refer to all three as a single "thing" called Cabal.

<div class="alert alert-block alert-info">
This lecture assumes you already have Cabal installed in your system; If you don't, please refer to the previous "Installing Haskell Locally" lesson. 
</div>

Cabal makes it easier for developers to:
- Use other developer's packages
- Create and share their own libraries
- Configure how to build and run tests and executables
- etc.

By default, Cabal uses [Hackage](https://hackage.haskell.org/) as the place to look for libraries needed by the developer.

Hackages is a central package archive that has thousands of Haskell libraries and programs ready for you to use them.

Cabal uses a local index of Hackage to resolve dependencies, so you always want to keep it updated. To do that, you can run:

```
cabal update
```

(See [here](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-update) the documentation.)

Now, we're ready to start our first Haskell project! 😄

### Creating a new Haskell project

To create a new Haskell project that follows the cabal architecture, we use the `cabal init` command:

```
cabal init
```

When we run this command, Cabal is going to start asking a bunch of questions about the software we want to build. **You can change everything later by modifying the `.cabal` file, so don't worry too much about it!**

After answering all those questions, Cabal will create files and folders inside your current directory. So make sure to create a directory and go inside before running `cabal init`!

Depending on your chosen options, you might have a different folder structure. But it should look mostly like this:

```
Project
    |-- app
    |    |-- Main.hs
    |-- CHANGELOG.md
    |-- Project.cabal
    |-- LICENSE
```

- The `Project` folder is the folder containing your Haskell project
- The `app` folder contains the source code for your application (by default, a single `Main.hs` file.
- The `CHANGELOG.md` file to record the changes between versions.
- The `Project.cabal` file contains all the configuration to build and execute your executables, library, and tests (if any).
- The `LICENESE` file containing the license of the software

Although this is likely how Cabal creates your project, it's common for Haskell projects to have the next folder structure:

```
Project
    |-- app
    |    |-- Main.hs
    |-- src
         |-- ...
    |-- CHANGELOG.md
    |-- Project.cabal
    |-- LICENSE
```

The difference is that most of the source code is inside the `src` folder, and the `app` folder only contains the `Main` action.

It doesn't make a real difference in the final result, so do as you please.

And now that we have our project ready to go, here there are a few key commands:

- `cabal build`: It builds your executable or library based on the contents of the source code and the `.cabal` file.
- `cabal exec Project`: It executes `Project`. The actual name of the executable is inside the `.cabal` file.
- `cabal run`: It runs `cabal build` and `cabal exec` in sequence. This is a convenient command to avoid the repetitive work of running the same two commands every time we change something.
- `cabal test`: Runs the tests specified in the `.cabal` file. (We won't work with tests in this lesson.)

As you can see, everything depends on the `.cabal` file. So let's learn about it!

### Going over the Cabal file

The `.cabal` file contains rules in a similar format as YAML files. Let's see the most common ones, starting with the informational:

```
cabal-version:      3.0
-- The cabal-version field refers to the version of the .cabal
-- specification and can be different from the cabal-install
-- (the tool) version and the Cabal (the library) version you
-- are using.
-- The way we write this (ForestGame.cabal) file changes over
-- time with new versions. By stating the version we use, Cabal
-- will be able to interpret it.
-- Starting from the specification version 2.2, the cabal-version
-- field must be the first thing in the cabal file.


-- The name of the package.
name:               ForestGame

-- The package version.
-- See the Haskell package versioning policy (PVP) for standards
-- guiding when and how versions should be incremented.
-- https://pvp.haskell.org
-- PVP summary:     +-+------- breaking API changes
--                  | | +----- non-breaking API additions
--                  | | | +--- code changes with no API change
version:            0.1.0.0

-- A short (one-line) description of the package.
synopsis:           A cool game

-- A longer description of the package.
description:        This game is really, reeeally cool! Trust me.

-- The license under which the package is released.
license:            MIT

-- The file containing the license text.
license-file:       LICENSE

-- The package author(s).
author:             Robertino Martinez

-- An email address to which users can send suggestions, 
-- bug reports, and patches.
maintainer:         robertino.martinez@iohk.io

-- Category to find our package if we upload it to Hackage
category:           Game

-- The type of build used by this package. Leave it as it is
-- or remove it entirely. We won't use Custom builds.
build-type:         Simple

-- Extra doc files to be distributed with the package, such
-- as a CHANGELOG or a README.
extra-doc-files:    CHANGELOG.md
```

As you can see, we're adding information about ourselves and the package, but no instructions on how to process the source code yet.

Now, let's see we want to build the executable of our game. For that, we can use the `executable` sections. Let's see one:

```
executable ForestGame
    main-is:          Main.hs
    other-modules:    Forest.Level1
                    , User.Actions.Move
    build-depends:    base   ^>=4.16.4.0
                    , random ^>=1.2.1.1
    hs-source-dirs:   app, src
    default-language: Haskell2010
```

We indicate what is part of the `executable` section using indentation. Let's go over one by one:

- `executable ForestGame`: Indicates that we're specifying an executable called ForestGame.
- `main-is`: We specify where the main module resides.
- `other-modules`: We specify auxiliary modules used by our executable.
- `build-depends`: Declares the library dependencies (with version) required to build the current package component. In this case, we depend on the `base` and `random` libraries. (see the video of this lesson to know how we use them.)
- `hs-source-dirs`: Root directories for the module hierarchy. In this case, instead of using only `app`, we chose to use `app` and `src` to contain our source code.
- `default-language`: The version of Haskell we want to use.

With only the informational and executable instructions, we have everything needed for Cabal to be able to build our executable.

Although, we also have the option to create:
- [A library](https://cabal.readthedocs.io/en/stable/cabal-package.html#library),
- [Tests suites](https://cabal.readthedocs.io/en/stable/cabal-package.html#test-suites),
- And more executables if we wanted to. (For example, if we wanted to have "development" and "production" executables that differ in some way.)


You can configure A LOT with Cabal. Usually, the best course of action is to figure out what you want to do and search [Cabal's documentation](https://cabal.readthedocs.io/en/stable/index.html) to see how to do it.

### Building and running our executable

Now that we have our cabal file, we can build and execute our software without worrying about resolving dependencies, downloading packages, etc.

We can just run:

```
cabal build
```

And Cabal will do everything for us! 😄🙌

And once everything is built, we can run any executable with:

```
cabal exec ForestGame
```

Of course, while developing, you'd have to build+execute quite a lot. That's why Cabal has a special command for that:

```
cabal run
```

When running `cabal run`, Cabal will check if any source code changed, and if it did, it will rebuild only the files that changed and then run the executable! Extremely convenient! 😎

<div class="alert alert-block alert-info">
For a more step-by-step explanation with a real-world example, take a look at the corresponding video lesson! 😃 
</div>

## Language extensions and Pragmas

Usually, in most mainstream programming languages, your language for a specific version always works the same. For example, Python 3.7 and 3.10 are different, but Python 3.7 will always have the same syntax and functionality. The only way you can change how Python works or its syntax is by changing the version of the language.

Haskell is different. Haskell has language extensions.

Language extensions are a way to modify how the Haskell compiler interprets your code. By activating language extensions, you essentially change the Haskell language to have a particular feature you want or need. This way, you can tailor the language to your taste or use case!

You can add language extensions by specifying them in your `.cabal` file or by adding a **Language Pragma** to the top of the file you want the extension to be active.

The syntax to add a language pragma to a Haskell file is by adding this statement at the top of the file:

```
{-# LANGUAGE extension_name #-}
```

Let's see a few examples!

### NumericUnderscores

Let's say we're working with an app that uses big numbers, such as a game:


```haskell
userGems = 15894231

purchase gems item = gems - item

level1Vest = 52000
level2Vest = 147000
level3Vest = 845000
tank = 314159265358

-- Usage example:
purchase userGems level3Vest
```


    15049231


It works. But because we're using large numbers, it's easy to mess up. It's hard to tell the number at a glance. One more zero looks kind of the same! 🤷‍♂️

That's when the `NumericUnderscores` language extension comes in.

This language extension adds support for expressing underscores in numeric literals. That is, underscores in numeric literals are ignored when `NumericUnderscores` is enabled. For instance, the numeric literal 1_000_000 will be parsed into 1000000 when `NumericUnderscores` is enabled. The underscores are only there to assist human readers.

So, if we activate the extension, we can rewrite our code like this:


```haskell
{-# LANGUAGE NumericUnderscores #-}

userGems = 15_894_231

purchase gems item = gems - item

level1Vest = 52_000
level2Vest = 147_000
level3Vest = 845_000
tank = 314_159_265_358

-- Usage example:
purchase userGems level3Vest
```


    15049231


As you can see, the final result didn't change. But we effectively changed Haskell's syntax so that numeric values can look different (can have underscores)!

Now, let's use a language extension that does a more significant change: `TypeApplications`.

### TypeApplications

`TypeApplications` allows you to instantiate one or more of a polymorphic function’s type arguments to a specific type.

For example, the function `read` is of type:


```haskell
:t read
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
</style><span class='get-type'>read :: forall a. Read a => String -> a</span>


 The `forall a.` part indicates that all `a` are the same type. And `Read a =>` indicates that the same type is an instance of the `Read` type class.
 
So, if we do:


```haskell
read "4"
```


    Prelude.read: no parse


In this JupyterNotebook, we get that `Prelude.read: no parse` error. But in a real scenario, we'd get a long error that starts with:

```
Ambiguous type variable ‘a0’ arising from a use of ‘parse’
  prevents the constraint ‘(Read a0)’ from being solved.
```

We get an error because the literal `4` could be any numeric value, and we're not indicating which one. So the compiler doesn't know which instance of `Read` to use. Should it use the `Int` instance? The `Integer` instance? `Float`? `Double`?

One way to solve this specific scenario is to use the `::` operator like this:


```haskell
(read "4") :: Int
```


    4


We're explicitly expressing that the `read "4"` expression should evaluate to an `Int`, so now the compiler knows it should use the `Read Int` instance.

Now, instead of doing this, we can use the `TypeApplications` language extension and use the `@` keyword to apply the function to a type like this:


```haskell
{-# LANGUAGE TypeApplications #-}

read @Int "4"
```


    4


What does means comes back to the `read` type signature. If we compare the type signature of `read` and `read @Int`:


```haskell
{-# LANGUAGE TypeApplications #-}

:t read
:t read @Int
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
</style><span class='get-type'>read :: forall a. Read a => String -> a</span>



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
</style><span class='get-type'>read @Int :: String -> Int</span>


We see that we're specializing the type variable `a` to `Int`.

This works in more complex scenarios. In the following example, we define the `Number` type that holds either a whole number, a decimal number, or a string of an unidentified number:


```haskell
{-# LANGUAGE TypeApplications #-}

data Number a b = WHOLE a | DECIMAL b | NAN String deriving (Show)

:t WHOLE
:t WHOLE @Int
:t WHOLE @Int @Float
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
</style><span class='get-type'>WHOLE :: forall a b. a -> Number a b</span>



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
</style><span class='get-type'>WHOLE @Int :: forall b. Int -> Number Int b</span>



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
</style><span class='get-type'>WHOLE @Int @Float :: Int -> Number Int Float</span>


And if you want to specify the type of the second parameter (`b`) but leave the first type as a polymorphic one, you can use another language extension called `PartialTypeSignatures` that allows you to put wildcards in types:


```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

data Number a b = WHOLE a | DECIMAL b | NAN String deriving (Show)

:t DECIMAL @Int
:t DECIMAL @_ @Float -- Now the "a" type is shown as "_".
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
</style><span class='get-type'>DECIMAL @Int :: forall b. b -> Number Int b</span>



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
</style><span class='get-type'>DECIMAL @_ @Float :: forall _. Float -> Number _ Float</span>


Let's see this in a way that makes a real difference in the final result.

We create the `parse` function that takes in a `String` and parses it into a value of type `Number a b` depending if it represents a `WHOLE` number, a `DECIMAL` number, or if it couldn't identify the number (`NAN`):


```haskell
parse :: (Read a, Read b) => String -> Number a b
parse inp
  | isNumber && isDecimal = DECIMAL $ read inp
  | isNumber = WHOLE $ read inp
  | otherwise = NAN inp
 where
  validChar = ".0123456789"
  isNumber = all (`elem` validChar) inp
  isDecimal = '.' `elem` inp
```

Now, if we try to use this function without indicating the type:


```haskell
parse "98"
```


    Prelude.read: no parse


We get the same error we got before because GHC can't infer the type of `98`. 

If we indicate the types, everything works fine. But not only that! **By specializing the variables `a` and `b` types when applying the `parse` function, we get to choose what kind of precision we want when parsing a whole or a decimal number!**:


```haskell
longWhole = "9223372036854775808"
longDecimal = "1.23456789"

--     'a'       'b'   
parse @Int             longWhole
parse @Integer         longWhole
parse @_       @Float  longDecimal
parse @_       @Double longDecimal
parse @Int     @Double "para bailar la bamba!!"
```


    WHOLE (-9223372036854775808)



    WHOLE 9223372036854775808



    DECIMAL 1.2345679



    DECIMAL 1.23456789



    NAN "para bailar la bamba!!"


#### The `@` symbol

The `@` symbol is also used to pattern-match an entire structure (like a list, record type, etc.) while pattern-matching for its parts.

In the example below, the `counts` function takes in a list and returns a list of tuples with the unique value and how many times the value is in the list.

When we pattern match to extract the first value `(x:_)`, we also add `list@` that creates a variable named `list`, which is equal to the entire list `x:xs`:


```haskell
counts :: Eq a => [a] -> [(a, Int)]
counts [] = []
counts list@(x:_) = (x, length (filter (== x) list)) : counts (filter (/= x) list)

counts "successfully"
```


    [('s',3),('u',2),('c',2),('e',1),('f',1),('l',2),('y',1)]


The main difference between when we use the `@` sign to apply a type or to pattern match is the space to is left: 
- Pattern matching: it has no space to its left `list@(x:xs)`
- Apply type: it has a space `read @Int`

That's how the compiler knows which is which.

There are a ton of language extensions ([see this list for reference](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/table.html)), and we'll use new ones when needed. But for now, make sure to do your homework!

<div class="alert alert-block alert-danger">
The homework is based on the example in the video lesson. If you feel lost, take a look at the part where I explain the example's code.
</div>

## That's it for today! Make sure to do the homework! 😁
