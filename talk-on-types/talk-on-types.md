---
title: "The Talk on Types"
author: Sandy Maguire
patat:
  wrap: true
  margins:
    top: 3
    left: 5
    right: 5
---

# The Talk on Types

---

* **Sandy Maguire**
* sandy@sandymaguire.me

. . .

* reasonablypolymorphic.com
* github.com/isovector
* leanpub.com/thinking-with-types

. . .

Today's slides:

* tinyurl.com/talk-on-types
* github.com/isovector/talks/blob/master/talk-on-types/talk-on-types.md

---

I'm currently traveling around the world, sleeping on couches and collaborating
with people.

. . .

Thanks Jonathan!

. . .

**Consider hosting me!**

* isovector.github.io/erdos/

---

## What are Types?

. . .

> "Specifications for how to layout data in memory."
>
> -- C programmers

---

## What are Types?

. . .

> "Unnecessary boilerplate >:("
>
> -- Javascript/Python/Ruby programmers

---

## What are Types?

. . .

> "Objects!!!"
>
> -- Java programmers

---

## What are Types?

. . .

Types are **program specifications.**

. . .

Types help us **weed out incorrect programs.**


---

## What are Types?

> "But Sandy! Most business logic bugs aren't type errors!"

. . .

**They can be.**

. . .

Most detractors of strong typing haven't internalized this fact.

---

## What are Types?

Bugs are insidious.

. . .

I'm going to try to sneak questionable things past you in this talk.

. . .

**Please keep it to yourself** if you spot one.

. . .

&nbsp;

Don't worry, I'll point them out soon enough!

---

## Modeling HTTP

```haskell
data Http = Http
  { url         :: String
  , verb        :: Verb
  , bodyContent :: String
  }


data Verb = GET | POST
```

---

## Modeling HTTP

```haskell
greatWebsite :: Http
greatWebsite = Http
  { url         = "https://sandymaguire.me"
  , verb        = GET
  , bodyContent = ""
  }
```

---

## Modeling HTTP

```haskell
greatWebsite :: Http
greatWebsite = Http
  { url         = "https://sandymaguire.me"
  , verb        = GET
  , bodyContent = ""  -- wtf?
  }
```

Not so nice!

---

## Modeling HTTP

```haskell
data Http = Http
  { url         :: String
  , verb        :: Verb
  , bodyContent :: String
  }
```

---

## Modeling HTTP

```haskell
data Http = Http
  { url         :: String
  , verb        :: Verb
  , bodyContent :: Maybe String
  }
```

. . .

```haskell
greatWebsite :: Http
greatWebsite = Http
  { url         = "https://sandymaguire.me"
  , verb        = GET
  , bodyContent = Nothing
  }
```

. . .

Much nicer!

---

## Setting Verbs

```haskell
setVerbToPost :: Verb -> Http -> Http
```

. . .

What does this thing do?

. . .

&nbsp;

> "Obviously it sets the `verb`."
>
> --Everyone

---

## Setting Verbs

```haskell
setVerbToPost :: Verb -> Http -> Http
```

What does this thing do?

&nbsp;

> "Obviously it sets the `verb`. Duh."
>
> --Everyone

---

## Setting Verbs

You're right!

. . .

```haskell
setVerbToPost (Http url _ _) =
    Http url POST Nothing
```

---

## Loading Content

What about this one?

```haskell
getContent :: Http -> IO Http
```

. . .

```haskell
-- Fetch the url if it's a GET
getContent (Http url GET _) = do
    contents <- fetchWebpage url
    pure (Http url GET contents)
```

. . .

```haskell
-- Otherwise do nothing
getContent http =
    pure http
```

---

## A Real Program

```haskell
main :: IO ()
main = do
  result <- getContent greatWebsite
  case (bodyContent result) of
    Just page -> putStrLn page
    Nothing   -> printStrLn "this can never happen"
```

---

## A Real Program

```haskell
main :: IO ()
main = do
  result <- getContent greatWebsite
  case (bodyContent result) of
    Just page -> putStrLn page
    Nothing   -> printStrLn "this can never happen"
    --           ^   annoying, but necessary to satisfy the typechecker
```

. . .

Presumably things like this are why people don't like types.

. . .

They *know things* about their code that the compiler *doesn't.*

---

**Rather than giving up on types...**

Let's just teach the compiler what we know!

. . .

&nbsp;

&nbsp;

&nbsp;

...but first...

---

**Did you spot the bug?**

. . .

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST Nothing
```

. . .

This *silently deletes* any loaded contents!!!!

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

. . .

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ contents) =
    Http url POST contents
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ (Just contents)) =
    Http url POST (Just (reverse contents))
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST Nothing
```
---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "a")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "b")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "agiasgd")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "dgjzADs")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "gds9gaz")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "something salacious")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "z9sj31a")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just "ls19z--")
```

---

## The Scope of the Problem

Consider some other implementations of `setVerbToPost`:

```haskell
setVerbToPost :: Http -> Http
setVerbToPost (Http url _ _) =
    Http url POST (Just url)
```

---

How can we prevent this sort of bug?

. . .

**Enlist the type system!**

. . .

```haskell
data Http   = Http
  { url         :: String
  , verb        :: Verb
  , bodyContent :: Maybe String
  }
```

---

How can we prevent this sort of bug?

**Enlist the type system!**

```haskell
data Http a = Http
  { url         :: String
  , verb        :: Verb
  , bodyContent :: a
  }
```

---

```haskell
setVerbToPost :: Http a -> Http a
```

. . .

```haskell
setVerbToPost (Http url _ _) =
    Http url POST Nothing
```

. . .

**This doesn't compile anymore!**

. . .

> Couldn't match expected type 'a' with actual type 'Maybe b'

---

```haskell
setVerbToPost :: Http a -> Http a



setVerbToPost (Http url _ _) =
    Http url POST Nothing
```

---

```haskell
setVerbToPost :: Http a -> Http a



setVerbToPost (Http url _ contents) =
    Http url POST contents
```

. . .

:)

---

## Why does this work?

Recall that in Haskell, there is *no way* to "zero-initialize" arbitrary types.

. . .

We simply can't summon up an `a` from nowhere.

. . .

Our *only choice* is to give back the one we were given.

---

## Why does this work?

As we saw, this is just not true for concrete types.

. . .

When a type is polymorphic, we can only reason about it "structurally."

---

## Why does this work?

Consider the function:

```haskell
foo :: [Double] -> Int
```

. . .

What might its implementation be?

---

## Why does this work?

```haskell
foo :: [Double] -> Int
foo = length
```

. . .

The type is too specific! We don't use the fact that it's a list of `Double`s
whatsoever!

---

## Why does this work?

```haskell
foo :: [a] -> Int
foo = length
```

. . .

This is much easier to reason about!

. . .

**Takeaway:** Polymorphic is often better than concrete.

---

## Bonus!

This also cleans up our program!

. . .

```haskell
main :: IO ()
main = do
  result <- getContent greatWebsite
  putStrLn $ bodyContents result
```

. . .

No more ugly case statement!

---

We can use our new type variable to help reason about `getContents` too!

. . .

```haskell
getContent :: Http a -> IO (Http String)
```

. . .

The variable `a` shows up on only the input side --- not the output.

. . .

This means that whatever `a` was there *must have been discarded.*



---

**Let's do something a little sexier.**

. . .

&nbsp;

&nbsp;

...but first...

---

**Did you spot the *other* questionable thing?**

. . .

```haskell
getContent :: Http -> IO Http
getContent (Http url GET _) = do
    contents <- fetchWebpage url
    pure (Http url GET contents)
getContent http = pure http
```

---

**Did you spot the *other* questionable thing?**

```haskell
getContent :: Http -> IO Http
getContent (Http url GET _) = do
    contents <- fetchWebpage url
    pure (Http url GET contents)
getContent http = pure http  -- hmm
```

. . .

We are *silently doing nothing* if the method is wrong!

---

## Fixing It

Doing nothing is probably the least bad thing we can do here.

. . .

But why are we even *allowed* to call `getContent` on a `POST` request?

. . .

If we couldn't do that, then we *wouldn't need any* semantics for that case.

---

## Fixing It

```haskell


data Http a = Http
  { url         :: String
  , verb        :: Verb
  , bodyContent :: a
  }

getContent    :: Http a -> IO (Http String)

setVerbToPost :: Http a -> Http a
```

---

## Fixing It

```haskell
{-# LANGUAGE DataKinds, KindSignatures #-}

data Http a = Http
  { url         :: String

  , bodyContent :: a
  }

getContent    :: Http a -> IO (Http String)

setVerbToPost :: Http a -> Http a
```

---

## Fixing It

```haskell
{-# LANGUAGE DataKinds, KindSignatures #-}

data Http (v :: Verb) a = Http
  { url         :: String

  , bodyContent :: a
  }

getContent    :: Http a -> IO (Http String)

setVerbToPost :: Http a -> Http a
```

---

## Fixing It

```haskell
{-# LANGUAGE DataKinds, KindSignatures #-}

data Http (v :: Verb) a = Http
  { url         :: String
  , bodyContent :: a
  }


getContent    :: Http 'GET a -> IO (Http 'GET String)

setVerbToPost :: Http a -> Http a
```

---

## Fixing It

```haskell
{-# LANGUAGE DataKinds, KindSignatures #-}

data Http (v :: Verb) a = Http
  { url         :: String
  , bodyContent :: a
  }


getContent    :: Http 'GET a -> IO (Http 'GET String)

setVerbToPost :: Http v a -> Http 'POST a
```

. . .

Now `POST`s are a completely different type than `GET`s.

. . .

&nbsp;

**It's a type-error to attempt to call `getContent` on a `POST`!**

---

## Thanks for listening!

. . .

Any questions?

&nbsp;

&nbsp;

&nbsp;


* **Sandy Maguire**
* sandy@sandymaguire.me

* reasonablypolymorphic.com
* github.com/isovector
* leanpub.com/thinking-with-types


