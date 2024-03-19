# Creating Parameterized and Recursive Types

## Outline
* Parameterizing Types
	* Prameteryzing `type` synonyms
	* Prameteryzing `data` types
* Recursive data types
    * `Tweet` me a river
    * A `Sequence` of `Node`s
    * A `Tree` of `Node`s
* Kinds
* The `newType` keyword

## Paremeterizing Types

A **value** constructor **takes values** as parameters and **produces a value**.

                            |
                            v

A **type** constructor **takes types** as parameters and **produces a type**.

We can use type constructors with both type synonyms and new types. Let's start with type synonyms.

### Parameterizing Type Synonyms

Going back to our last type synonym, we had:


```haskell
type Name = String
type Address = (String, Int)

type Person = (Name, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Person
```

Imagine that, after using it for a while, we find out that we also have to identify companies by their numeric id and providers by their alphanumeric id.

We could do something like:


```haskell
type Address = (String, Int)
type Name = String
type CompanyId = Int
type ProviderId = String

type Person = (Name, Address)
type Company = (CompanyId, Address)
type Provider = (ProviderId, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Person
io = (584264, ("Cardano St.", 999)) :: Company
google = ("Google LLC", ("Amphitheatre Parkway", 1600)) :: Provider
```

In this case, we add four more type synonyms. The `CompanyId`, `ProviderId`, `Company`, and `Provider` synonyms.

We get our desired result, but at the expense of repeating the same structure three times (`Person`, `Company`, and `Provider` are tuples with something and an `Address`). A different approach would be to define a parametric type synonym.

For example, we could create the `Entity a` type:


```haskell
type Entity a = (a, Address)
```

To define a parametric type synonym, we have to indicate the parameter to the left of the `=` sign and use it on the right. Same as with functions.

And now, every time we use `Entity a`, we can adjust the type of `a` according to our needs. For example:


```haskell
type Name = String
type Address = (String, Int)
type CompanyId = Int
type ProviderId = String

type Entity a = (a, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Entity Name 
io = (584264, ("Cardano St.", 999)) :: Entity CompanyId
google = ("Google LLC", ("A. Parkway", 1600)) :: Entity ProviderId
other = (True, ("Some street", 0)) :: Entity Bool
```

This time, we added just three more type synonyms. The `CompanyId`, `ProviderId`, and `Entity a` synonyms.

And below, we have four different values with four different types. All of them are obtained by passing a different type to the same type constructor.

Notice that:
* `Entity` by itself is a type constructor, not a type, so **no value can have a type of just `Entity`**.
* `Entity Name`, `Entity CompanyId`, `Entity ProviderId`, and `Entity Bool` are completely different types!

We can also use multiple parameters. For example, the `Address` type synonym is also a pair. But one that doesn't have `Address` as the second element. So we could generalize `Entity a` even more and convert the two values into parameterized types:


```haskell
type Name = String
type Address = Entity String Int
type CompanyId = Int
type ProviderId = String

type Entity a b = (a, b)

bob = ("Bob Smith", ("Main St.", 555)) :: Entity Name Address 
io = (584264, ("Cardano St.", 999)) :: Entity CompanyId Address
google = ("Google LLC", ("A. Parkway", 1600)) :: Entity ProviderId Address
other = (True, ("Some street", 0)) :: Entity Bool Address
```

As you can see, now `Entity a b` takes two type parameters. And `Address` is a synonym to a specific case of `Entity a b` where the first parameter is a `String` and the second is an `Int`.

Of course, now the name `Entity` doesn't make much sense, and our types are starting to get convoluted. I just wanted to show you that you could use more than one type parameter, and it's not a big deal. But that's pretty much it for type synonyms. They are useful to provide extra context when needed, and they provide certain flexibility allowing for type parameters. But other than that, they're pretty boring.

Let's learn the good stuff! You know what I mean. Let's learn about parameterizing data types!

### Parameterizing data types

To add type parameters while defining new types, you do the same as with function and parameterized type synonyms. Add the parameter to the left of the `=` sign, and (optionally) use it on the right:


```haskell
data Box a = Empty | Has a deriving (Show)
```

Here, we're defining a brand new type. A type that represents a box that contains values.

In this case, `Box` is a type constructor that takes one type variable `a`. So we could have values of type `Box Bool`, `Box Char`, `Box Float`, etc.

And we have two value constructors:


```haskell
:t Empty
:t Has
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
</style><span class='get-type'>Empty :: forall a. Box a</span>



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
</style><span class='get-type'>Has :: forall a. a -> Box a</span>


`Empty` for when the box is empty. In this case, `Empty` is of type `Box a`, meaning it is polymorphic. We don't know the type of what it's inside because it's empty!

And the `Has` value constructor for when the box has a value inside. In this case, we do have a value inside, so the type of `Box a` will be dependent on the type of that value.

For example:


```haskell
box1 = Has "What's in the box?!"
:t box1

box2 = Empty
:t box2
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
</style><span class='get-type'>box1 :: Box [Char]</span>



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
</style><span class='get-type'>box2 :: forall a. Box a</span>


We can also modify and combine the values inside the boxes:


```haskell
-- data Box a = Empty | Has a

box = Has (1 :: Int)

addN :: Num a => a -> Box a -> Box a
addN _ Empty   = Empty
addN n (Has a) = Has (a + n)

addN 3 box
```


    Has 4



```haskell
-- data Box a = Empty | Has a

addBoxes :: Num a => Box a -> Box a -> Box a
addBoxes _ Empty = Empty
addBoxes Empty _ = Empty
addBoxes (Has a) (Has b) = Has (a + b)

addBoxes (Has 3) (Has 7)
addBoxes (Has 5) Empty
```


    Has 10



    Empty


And what if we want to extract the value inside the box? The case of the `Has a` value constructor is easy, we just need to pattern match and return `a`. But what about the case when the box is empty?

Well, we could ask for a default value to return if the box is empty. That way, we always return a value!

So, if we translate this to code, we get:


```haskell
-- data Box a = Empty | Has a

extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x

extract 'a' Empty
extract 0 (Has 15)
extract 0 Empty
extract [] (Has [1,2,3,4])
```


    'a'



    15



    0



    [1,2,3,4]


We could keep creating more functions for `Box a`, but there's still a lot to cover, so let's keep going!

We can also use type constructors with record syntax. Imagine we also want the option of using other ways of representing colors in our shapes. Previously, we used `String` values and wrote down the color's name. But other situations could warrant different formats. Like hexadecimal or RGB values. So, better if we parameterize our type like this:


```haskell
data Shape a
  = Circle
      { position :: (Float, Float)
      , radius   :: Float
      , color    :: a
      }
  | Rectangle
      { position :: (Float, Float)
      , width    :: Float
      , height   :: Float
      , color    :: a
      }
  deriving (Show)
```

Now, the color field can be of any type, and our shape can be of type `Shape String`, `Shape Int`, etc.

For example:


```haskell
circleS = Circle { position = (1,2), radius = 6, color = "Green"}
:t circleS

type RGB = (Int,Int,Int)
circleRGB = Circle { position = (1,2), radius = 6, color = (0, 128, 0) :: RGB}
:t circleRGB
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
</style><span class='get-type'>circleS :: Shape [Char]</span>



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
</style><span class='get-type'>circleRGB :: Shape RGB</span>


And all the other properties of record types still apply.

Now that we know all these ways of creating types, we'll go through a few more examples to hone in on the knowledge. But because we're highly efficient students, we'll kill two birds with one stone and learn about recursive while at it!

## Recursive data types

We can use type synonyms inside other type synonyms. But for technical reasons, we cannot define recursive type synonyms. We can, though, define recursive data types.

### Tweet me a river

Here's the situation. Elon musk wants to rebuild Twitter using Haskell. And you're interviewing for the position. The first question is to define a data type for a tweet. A tweet has its contents, the number of retweets, likes, comments, metadata, etc. That would be a huge data type, but the interviewer doesn't care about the details. He wants you to present the general idea.

So, you provide this:


```haskell
data Tweet = Tweet
  { contents :: String
  , likes :: Int
  , comments :: [Tweet]
  }deriving (Show)
:t Tweet -- Type of the Tweet constructor
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
</style><span class='get-type'>Tweet :: String -> Int -> [Tweet] -> Tweet</span>


Just 1 constructor with 3 fields. You used record syntax because you know this type will eventually contain many more fields, and it would be cumbersome to use regular syntax. You also figured out that a comment to a tweet is just another tweet, so you can recursively use `[Tweet]` as the type of the comments inside the `Tweet` data type.

And to test it out, you create a realistic `Tweet` value: 


```haskell
tweet :: Tweet
tweet = Tweet "I'm angry about something! >.<" 5
    [ Tweet "Me too!" 0 []
    , Tweet "It makes me angry that you're angry" 2
        [ Tweet "I have no idea what's happening" 3 [] ]
    ]
```

The interviewer liked your idea but was skeptical about how easy it would be to work with a type like this. And to prove that it's super easy, you wrote a function to measure the engagement based on the number of likes and responses the tweet and all the tweets that tweet generated had:


```haskell
engagement :: Tweet -> Int
engagement Tweet {likes = l, comments = c} = l + length c + sum (map engagement c)

engagement tweet
```


    13


The `engagement` function pattern matched only the fields it needed, then it added the likes and amount of comments of that tweet. And to that, it added the sum of numbers generated by recursively mapping the `engagement` function we're creating to all the tweets on the list of comments.

The interviewer is so impressed that she stops the interview short and offers you a senior position. But you rejected the offer when you found out that, now, the salaries of all Twitter employees are paid in Dodge coin.

So, you move on to the next adventure.

### A `Sequence` of `Nodes`

After successfully completing Twitter's interview process and rejecting their offer, your confidence is skyrocketing, and you decide to give it a go at Google.

The initial interviews are ok, but it's time for the real deal: The technical interview! You show up on time, and so does the interviewer. We're up for a good start. And here comes the first question:

"Write data type that represents a linear sequence of nodes where each node contains a value and points to the rest of the sequence."

Easy enough! So, you need a data type similar to the `Box a` we created before:


```haskell
data Box a = Empty | Has a
```

The `Empty` constructor represents an empty node, and the `Has` constructor is a node that has a value inside. That's a good start. The problem is that you need to contain a sequence of these boxes. 

Luckily, you know you can pass multiple parameters to a value constructor, so you can simply add another box as the second parameter of the `Has` constructor:


```haskell
data Box a = Empty | Has a (Box a)

:t Has
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
</style><span class='get-type'>Has :: forall a. a -> Box a -> Box a</span>


That new parameter means that the `Has` value constructor now contains a value and a box that can contain another value and another box, and so on and so forth.

And with that, boom! We have a data type that is a linear sequence of boxes (or nodes) where each box has a value and points to the rest of the boxes.

And that's awesome! But you did all this in your head, and the interviewer started to get worried about your long silence. So you explained the reasoning but changed the word "Box" with "Sequende" and "Has" with "Node" because that's the language of the question. So you presented the solution like this:


```haskell
data Sequence a = EmptyS | Node a (Sequence a) deriving (Show)

:t Node
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
</style><span class='get-type'>Node :: forall a. a -> Sequence a -> Sequence a</span>


That data type represents a sequence of nodes that could be either empty or have a node that contains a value and points to the rest of the sequence. It's the same type as before, but changing the names makes you think differently about what's happening. 

And to prove that it works as expected, you create a few values:


```haskell
-- data Sequence a = EmptyS | Node a (Sequence a)

sequence1 :: Sequence a
sequence1 = EmptyS -- A sequence of just one empty node

sequence2 :: Sequence Char
sequence2 = Node 'a' EmptyS -- A sequence of 2 nodes

sequence3 :: Sequence Bool
sequence3 = Node True (Node False EmptyS) -- A sequence of 3 nodes

sequence4 :: Sequence Integer
sequence4 = Node 1 (Node 2 (Node 3 EmptyS)) -- A sequence of 4 nodes
```

Right after that, the interviewer looked you dead in the eye and asked:

"And how is this useful?"

You hesitated for a second. And that's when you vaguely remembered a Haskell course you did a long time ago—yes, video lectures can be recursive too. You smiled and said, "oh, I'll tell you how useful this is."

And proceeded to slightly modify the data type to make a point. This is what you did:


```haskell
infixr 5 :->
data Sequence a = EmptyS | a :-> (Sequence a) deriving (Show)

:t (:->)
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
</style><span class='get-type'>(:->) :: forall a. a -> Sequence a -> Sequence a</span>


Since value constructors are just functions, you can also create infix value constructors—with the caveat that they have to start with a colon (`:`).

In this case, you define the `:->` (weird arrow) value constructor that takes the node's value as a first argument and the rest of the sequence as a second argument.

So the previous `sequence4` value now looks like this:


```haskell
sequence4 :: Sequence Integer
sequence4 = 1 :-> 2 :-> 3 :-> EmptyS -- A sequence of 3 nodes + empty node
```

Looks familiar? Exactly! That's a list!! If we compare the two side by side, it's pretty obvious:


```haskell
sequence4' :: [] Integer     -- Same as [Integer]
sequence4' = 1 : 2 : 3 : []  -- A list with 3 elements + empty list
```

And if we compare our type with how lists are defined in Haskell:

```haskell
data Sequence a = EmptyS | a :-> (Sequence a)

data []       a = []     | a :   [a]
```

We see that they are virtually the same type, but lists have some special "extra suggary" syntax to make them easier to use.

And that's why you chose the fixity to be `infixr 5`. Because it's the same that the `:` constructor.

After presenting that evidence, the utility of the type is obvious. You just recreated the list type, and lists are everywhere!

The interviewer was pleased, but he was just starting! And he asked:

"Now write a function to check if a specific element is inside this sequence."

The interviewer was pleased, but he was just starting! And he asked:

No problem! You had to implement the `elem` function for your new type the same way it's implemented for lists:


```haskell
-- data Sequence a = EmptyS | a :-> (Sequence a)

elemSeq :: (Eq a) => a -> Sequence a -> Bool
elemSeq _ EmptyS = False
elemSeq x (y :-> ys) = x == y || elemSeq x ys
```

You define the `elemSeq` function that takes a value of type `a` and a value of type `Sequence a` and returns a `Bool`. Where `a` is an instance of the equality type class (because you'll be chequing for equality).

You have two constructors, so you start with two equations. One for the `EmptyS` constructor and one for the `:->` constructor.

If the sequence is empty, you don't care about the other value because you know it won't be inside an empty node.

And if the sequence has at least one non-empty node, you pattern match to extract the value of the first node (`y`), check if it's equal to the value provided as the first parameter (`x`), and recursively apply the `elemSeq` function to the same initial value and the rest of the list.


If at least one element of the list is equal to the value provided, you want to return `True`. So, you use the `||` operator that takes two booleans and returns `True` if either is `True`. That way, as soon as you get one match, you'll get `True` till the end. And you will know that value is inside the sequence.

Using this function, we can check if an element is inside our sequence of nodes like this:


```haskell
seq5 = 'a' :-> 'b' :-> '4' :-> '%' :-> EmptyS

elemSeq 'c' seq5
elemSeq '%' seq5
```


    False



    True


"Well done." - says the interviewer - "But I have a problem with this. I have tens of thousands of elements, and if we have to check one by one in sequence, it'll take forever!"

You saw this coming from a mile away and said: "No problem! If we have the values ordered, we could use a Binary Search Tree!"

### A `Tree` of `Node`s

The interviewer was right! Imagine you have 10.000 items to go through. If you go one by one, it will take forever! So, what do you do?

Think about the last time you looked for a word in the dictionary. No, not on the computer. I mean an actual physical dictionary. How did you do it? Did you go to the first page, look for the word there, then to the second page, and so on and so forth? No! You straight up opened the dictionary in the middle! And when you saw that the word wasn't there, you chose one of the halves based on the order of the alphabet, split that half in half, and checked for the word again. That way, every time you checked, you reduced the size of the problem in half. That's called a "binary search algorithm," and it's much better than linear search. How much better, you ask?

For example, if the dictionary has 10.000 pages, when searching linearly, the worst-case scenario (the word is at the very end) would be to check all 10.000 pages. But if we use the binary search algorithm, the worst-case scenario would be that we need to check 13 pages! 13! That's it! You can see how this is a game changer for efficiency.

So, we want to create a data structure that allows us to easily search that way. There are a few we could use. But one of the most famous ones is the Binary Search Tree (also called the Sorted Binary Tree) data structure. And it looks like this:

<img style="float: left; position: relative; left: 20%;" alt="Binary Search Tree" src="../images/BST.png"/>

In a Binary Tree:
* Each node can have at most two child nodes
* It has only one root, that is, a node without a parent (node 8, in this case).
* And has only one path to get to any node.

So, the node of value 3 is the child of node 8 and the parent of nodes 1 and 6. And the only way to get to node 7 is through 8, 3, 6, and 7.

That's a binary tree. Now, what makes this "Binary Tree" a "Binary Search Tree" is that the value of each node is greater than all the values under the node's left subtree and smaller than the ones under its right subtree. For example, all the values under node 8's left subtree are smaller than 8, and all the values under node 8's right subtree are larger than 8.

By knowing this, each time we check the value of a node, and it's not the one we're looking for, we know that if the value is smaller, we have to keep looking on the left subtree, and if it's bigger, we have to keep going on the right subtree. Allowing us to discard all the nodes of the other branch and reducing the size of the problem in half. Same as we did in the dictionary example.

Ok, so how do we translate this to code? Well, Haskell makes it surprisingly easy. 


```haskell
data Sequence a = EmptyS | Node a (Sequence a) deriving (Show)
```

In our `Sequence a` type, we had one case where the node was empty and one when the node had a value and pointed to the rest of the sequence.

To make a BST, we need virtually the same type, except that now we want them to point to up to two sequences that are now trees. So the data type we need is this one:


```haskell
data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving (Show)

:t Node
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
</style><span class='get-type'>Node :: forall a. a -> Tree a -> Tree a -> Tree a</span>


And that's it! The only difference lies in the `Node` constructor, which now contains a value and two different subtrees. 

Let's plant a few trees:


```haskell
-- data Tree a = EmptyT | Node a (Tree a) (Tree a) 

emptyTree :: Tree a
emptyTree = EmptyT

oneLevelTree :: Tree Char
oneLevelTree = Node 'a' EmptyT EmptyT

twoLevelTree :: Tree Integer
twoLevelTree = Node 8
  (Node 3  EmptyT EmptyT)
  (Node 10 EmptyT EmptyT)

threeLevelTree :: Tree Integer -- Almost the same as the tree of the image
threeLevelTree = Node 8
  (Node 3
    (Node 1 EmptyT EmptyT)
    (Node 6 EmptyT EmptyT)
  )
  (Node 10
    EmptyT
    (Node 14 EmptyT EmptyT)
  )
```

Awesome. We have our data type ready to rock! We now need to implement the function to check if an element is inside a tree.

We start, as always, with the type. The function will take a value of type `a` and a tree of values of type `a`. It will check if the value is inside the tree and return a `Bool` of value `True` if it is and `False` if it isn't. So we can start with a type signature like this one:

```haskell
elemTree :: a -> Tree a -> Bool
```

Now, because the `Tree` type has two constructors, we know that it's likely we'll need two definitions (one per constructor) as the two cases. One for when the tree is empty, and one for when it's not:

```haskell
elemTree :: a -> Tree a -> Bool
elemTree v EmptyT = False
elemTree v (Node x left right) = ...
```

If the tree is empty, the value we provided is obviously not inside the tree, so we return `False`.

And what if the tree is not empty? Well, we just pattern-matched the node and have its value right there. Might as well check if it's the one we need:

```haskell
elemTree :: (Eq a) => a -> Tree a -> Bool
elemTree v EmptyT = False
elemTree v (Node x left right) = if v == x then True else ...
```

Because we are checking if the value of the first parameter is equal to the value inside the tree, we know the type `a` has to be an instance of the `Eq` type class. So we add that constraint to the signature.

If it is equal, we return `True` and end of the story. But if it's not, we have to choose the subtree to keep looking. And that depends if the value is bigger or smaller than the one in the node. So we not only have to check if the value is equal but also greater or smaller than the value of the node.

```haskell
elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree v EmptyT = False
elemTree v (Node x left right)
    | v == x = True
    | v > x  = ...
    | v < x  = ...
```

Because we now also have to use the `>` (greater than) and `<` (smaller than) behaviors, the types have to be an instance of the `Ord` type class. And because (like we saw in a previous lesson) to be an instance of the `Ord` type class, you have to previously be an instance of the `Eq` type class, we can remove that constraint and put the `Ord` constraint.

Also, because we'd need a bunch of nested if-else statements, we switch to guards for a more straightforward code. And now for the final two cases:


```haskell
elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree v EmptyT = False
elemTree v (Node x left right)
    | v == x = True
    | v > x  = elemTree v right
    | v < x  = elemTree v left 
-- Examples
elemTree 6 threeLevelTree
elemTree 17 threeLevelTree
```


    True



    False


If the value provided is bigger than the value of the node, we know that—if the value is in the tree—it will be in the right branch, where all the values are bigger than the value of the current node. So, the only thing we have to do is to recursively check the right subtree with the same initial value.

And if the value is smaller than the value for the node, we know that—if the value is in the tree—it will be in the left branch, where all the values are smaller than the value of the current node. So, the only thing we have to do is to recursively check the left subtree with the same initial value.

And that's it! We have a way to check if a value is in our data structure using the binary search algorithm.

That's a great solution. The thing is, while you were thinking about all this, you got so focused on your thoughts that you never noticed 15 minutes had passed without you saying anything! The interviewer got a bit scared and told you that it was great meeting you and they'll communicate to let you know if you passed the interview.

So, the takeaway is, in your next interview, remember to think out loud while working on the problems. It helps the interviewer know your thought process, and you avoid showing the face you make when binge-watching Haskell videos. Yes, the one you're making right now.

But don't worry, you'll have more hypothetical opportunities. For now, we still have a few more things to see today. For example, the fact that the shape of the data type directs how you write functions with it.

### The shape of the data type directs how you write functions with it

Now, this is not written in stone, but in general, you have one equation per value constructor. And if a constructor is recursive (one or N times), the equation will be recursive (one or N times).

A few examples are:


```haskell
-- data Box a = Empty | Has a

extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x
```

The `Box a` data type has two constructors (`Empty` and `Has`), and none are recursive.

So, when you write a function for this data type, it's likely you'll need to write two formulas (meaning two definitions)—one per constructor—and no formula will have a recursive call.


```haskell
-- data Sequence a = EmptyS | a :-> (Sequence a)

elemSeq :: (Eq a) => a -> Sequence a -> Bool
elemSeq _ EmptyS = False
elemSeq x (y :-> ys) = x == y || elemSeq x ys
```

The `Sequence a` data type has two constructors (`EmptyS` and `:->`), and one of the constructors (`:->`) is recursive (has `(Sequence a )` as a second parameter).

So, when you write a function for this data type, it's likely you'll need to write two formulas—one per constructor—and the formula that matches for the `:->` constructor will have a recursive call of the function you're defining.


```haskell
-- data Tree a = EmptyT | Node a (Tree a) (Tree a)

elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree v EmptyT = False
elemTree v (Node x left right)
    | v == x = True
    | v > x  = elemTree v right
    | v < x  = elemTree v left
```

The `Tree a` data type has two constructors (`EmptyT` and `Node`), and one of the constructors (`Node`) is two times recursive (`(Tree a )` twice).

So, when you write a function for this data type, it's likely you'll need to write two formulas—one per constructor—and the formula that matches the `Node` constructor will have two recursive calls of the function you're defining.

Of course, there are cases when this rule of thumb doesn't apply. but you can use it to get started whenever you're unsure how to define a function.

As you can see, there's so much going on with types, value constructors, type constructors, etc. that it's hard to keep track of things. Thankfully, Haskell has a trick up its sleeve: **Kinds**! 

## Kinds

Let's go back to the simpler days. Remember the `Box` type? No? Let's see.. it had a value constructor called `Has`. Let's check its type:


```haskell
:t Has
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
</style><span class='get-type'>Has :: forall a. a -> Box a</span>


Cool, so it takes a value of any type and returns a value of type `Box a`. And what's up with that `Box` type? How can I know more about it? If you try to check the type of a type, you get an error:


```haskell
:t Box
```


    <interactive>:1:1: error: Data constructor not in scope: Box


But there's a way to know more about that type. We can use the `:i` (info) command:


```haskell
:i Box
```




<div style='background: rgb(247, 247, 247);'><form><textarea id='code'>type Box :: * -> *
data Box a = Empty | Has a
  	-- Defined at <interactive>:1:1
</textarea></form></div><script>CodeMirror.fromTextArea(document.getElementById('code'), {mode: 'haskell', readOnly: 'nocursor'});</script>


The second line is the definition. But what in the world are those stars at the first line? That's the `Box`'s kind. Same as how the type of a value constructor gives you the quantity and the type of the values it takes, the kind of a type constructor gives you the quantity and kind of types it takes.

Let me say that again:

The **type** of a **value constructor** gives you the quantity and **type of the values** it takes.

                              |
                              v

The **kind** of a **type constructor** gives you the quantity and **kind of types** it takes.

So, **a kind is like the type of a type**.

You can read kinds like this:

* `*` means: **"concrete type"** (a type that doesn't take any parameters. Like `Float`.)
* `* -> *` means: **"type constructor that takes a single concrete type and returns another concrete type"** (Like `Box a`.)
* `* -> (* -> *) -> *` means: **"type constructor that takes one concrete type, and one single-paramter type constructor, and returns a concrete type"** (we haven't seen one of these yet.)
* And so on...

A few examples:

`Int`, `String`, and other like them are concrete types.


```haskell
-- Concrete types

:k Int 
:k String
:k Bool
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
</style><span class='get-type'>Int :: *</span>



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
</style><span class='get-type'>String :: *</span>



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
</style><span class='get-type'>Bool :: *</span>


As you can see, you can also check the kind of a type by using the `:k` (`:kind`) command.

`Box`, `Sequence`, and `Tree` all take a concrete type (`String`, `Int`, doesn't matter) and return a concrete type (`Box Int`, `Sequence String`, `Tree Float`).


```haskell
-- Type constructor with one concrete type as parameter

:k Box 
:k Sequence
:k Tree
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
</style><span class='get-type'>Sequence :: * -> *</span>



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
</style><span class='get-type'>Tree :: * -> *</span>


The `Entity` type synonym took two concrete types and returned a concrete type (`Entity String Bool`).


```haskell
-- Type constructor with two concrete types as parameters
-- type Entity a b = (a, b)

:k Entity
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
</style><span class='get-type'>Entity :: * -> * -> *</span>


As you can see, type synonyms also have kinds. Because they can also have type parameters.

And also, notice that as soon as a type constructor gets all its parameters, it becomes a concrete type:


```haskell
:k Box
:k Box String
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
</style><span class='get-type'>Box String :: *</span>


And that you can also partially-apply type constructors, the same as with functions or value constructors!:


```haskell
data DoubleBox a b = Empty | Has a b deriving (Show)

:k DoubleBox
:k DoubleBox String
:k DoubleBox String Int
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
</style><span class='get-type'>DoubleBox :: * -> * -> *</span>



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
</style><span class='get-type'>DoubleBox String :: * -> *</span>



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
</style><span class='get-type'>DoubleBox String Int :: *</span>


So, next time you need to know a bit more about a type constructor, check its kind!

And now, to finish the lecture, I'll give you one more teeny-tiny piece of information. But don't worry. You don't have to learn anything more than a caveat and a single keyword. And that's the `newType` keyword.

## The `newType` keyword

`newType` works essentially the same as the `data` keyword, except for an important caveat:

Types created with `newType` need to have exactly **one constructor** with exactly **one parameter/field**.


```haskell
-- Like this:
newtype Color a = Color a
-- And this:
newtype Product a = Product { getProduct :: a }
```

But, you can also do that with `data`. So, why use `newType`? 

Short version: **Performance reasons.** So, if you happen to be creating a data type with one constructor and one parameter, you can switch the `data` keyword to the `newtype` and get a performance boost for free.

# That's it for today!
