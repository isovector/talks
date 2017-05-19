:title: Some1 Like You
:data-transition-duration: 150

:css: fonts.css
:css: presentation.css



















----

:id: title

.. raw:: html

  <script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>

  <h1>Some1 Like You</h1>
  <h2>Dependent Pairs in Haskell</h2>
  <h3>A talk by <span>Sandy Maguire</span></h3>
  <h4>reasonablypolymorphic.com</h4>

----

An (almost) real life example.
==============================

We will make a simple data-ingestion platform for our life-tracking app.

The program will provide an API with a unique endpoint for each distinct type of data we can ingest.

----

Survey says: big growth!
========================

The marketing team says that by the end of the year, we'll have over 500 different "events" we'll want to be able to
ingest.

----

At this scale, *any* boilerplate is bad news.
=============================================

Boilerplate is boring to write and easy to get wrong.

----

A first attempt.
================

.. code:: haskell

  data Event = WakeUp
             | Eat     Meal
             | RockOut Song Duration


----

.. code:: haskell

  instance FromJSON Event where
    parseJSON = parseWakeUp
            <|> parseEat
            <|> parseRockOut


----

The API.
========

.. code:: haskell

  type Req  = ReqBody '[JSON] Value
  type Resp = Post    '[JSON] Response

  type EventAPI =
    "api" :> "event" :>
               ( "wake-up"  :> Req :> Resp
            :<|> "eat"      :> Req :> Resp
            :<|> "rock-out" :> Req :> Resp
               )


----

.. code:: haskell

  importEvent :: Value -> ExceptT ServantErr IO Response
  importEvent blob =
    case fromJSON blob of
      Error   err -> throwM err
      Success ev  -> pure $ Response ev


----

.. code:: haskell

  wakeUp  = importEvent
  eat     = importEvent
  rockOut = importEvent

  eventServer :: Server EventAPI
  eventServer = serve $
    wakeUp :<|> eat :<|> rockOut


----

Notice how there is no type safety here.

Our *wake-up* endpoint will happily accept a *eat* payload.

----

We can do better!
=================

Separate the constructors of our sum type into their own types.

----

.. code:: haskell

  data PayloadWakeUp  = PayloadWakeUp
  data PayloadEat     = PayloadEat     Meal
  data PayloadRockOut = PayloadRockOut Song Duration

  instance FromJSON PayloadWakeUp
  instance FromJSON PayloadEat
  instance FromJSON PayloadRockOut


----

.. raw:: html

  <pre>
  {-# LANGUAGE TemplateHaskell #-}

  data Event = EventWakeUp  <span class="new">PayloadWakeUp</span>
             | EventEat     <span class="new">PayloadEat</span>
             | EventRockOut <span class="new">PayloadRockOut</span>

  <span class="new">makePrisms ''Event</span>

  </pre>


----

Optics provide type safety!
===========================

We can use these prisms to lift our payload types into our `Event` type.

----

.. raw:: html

  <pre>
  {-# LANGUAGE RankNTypes #-}

  importEvent :: <span class="new">FromJSON e</span>
              <span class="new">=> Prism' Event e</span>
              -> Value
              -> ExceptT ServantErr IO Response
  importEvent <span class="new">prism</span> blob =
    case fromJSON blob of
      Error   err -> throwM err
      Success e   -> pure . Response $ <span class="new">review prism</span> e

  </pre>


----

Server upgrades.
================

.. raw:: html

  <pre>
  wakeUp  = importEvent <span class="new">_PayloadWakeUp</span>
  eat     = importEvent <span class="new">_PayloadEat</span>
  rockOut = importEvent <span class="new">_PayloadRockOut</span>

  eventServer :: Server EventAPI
  eventServer = serve $
    wakeUp :<|> eat :<|> rockOut

  </pre>


----

We've gained type safety!
=========================

The endpoints will no longer accept payloads of the wrong type.

----

The compiler doesn't know that our new payload types are related.

----

We can do better!
=================

Grouping our payload types together might provide opportunities for more clever tricks.

----

A brief interlude.
==================

On data kinds and type families.

----

Data kinds lifts *values* to **types**, and *types* to **kinds**.

Wat?

----

.. code:: haskell

  data Bool = True
            | False


.. raw:: html

  <pre>

  </pre>



begets, via DataKinds:

.. raw:: html

  <pre class="highlight code haskell">

  <span class="kc">kind</span> <span class="kind">Bool</span> where
    <span class="kc">type</span> '<span class="type">True</span>
    <span class="kc">type</span> '<span class="type">False</span>

  </pre>


----

Type families.
==============

A **type family** is a function that returns a type.

Type families only exist at the type level.

----

We can write type families over DataKinds.

----

Back to our regularly scheduled talk.
=====================================

----

.. code:: haskell

  {-# LANGUAGE DataKinds    #-}
  {-# LANGUAGE TypeFamilies #-}

  data EventType = WakeUp | Eat | RockOut


.. raw:: html

  <pre class="highlight code haskell">
  <span class="kc">data family</span> <span class="kt">Payload</span> (<span class="type">e</span> :: <span class="kind">EventType</span>)

  </pre>


----

.. raw:: html

  <pre>
  data <span class="new">instance Payload '<span class="type">WakeUp</span></span>  = WakeUp
  data <span class="new">instance Payload '<span class="type">Eat</span></span>     = Eat Meal
  data <span class="new">instance Payload '<span class="type">RockOut</span></span> = RockOut Song Duration

  instance FromJSON (Payload 'WakeUp)
  instance FromJSON (Payload 'Eat)
  instance FromJSON (Payload 'RockOut)

  </pre>


----

Data types for free.
====================

Armed with this type family, we can get our old sum type for free.

----

.. code:: haskell

  {-# LANGUAGE GADTs #-}

  data Event where
    MkEvent :: Payload (et :: EventType) -> Event


----

.. raw:: html

  <pre>
  <span class="new">{-# LANGUAGE AllowAmbiguousTypes #-}</span>
  <span class="new">{-# LANGUAGE KindSignatures      #-}</span>
  <span class="new">{-# LANGUAGE ScopedTypeVariables #-}</span>

  importEvent :: <span class="new">forall (<span class="type">et</span> :: <span class="kind">EventType</span>)</span>
               . FromJSON (Payload <span class="type">et</span>)
              -> Value
              -> ExceptT ServantErr IO Response

  importEvent blob =
    case fromJSON blob of
      Error err ->
        throwM err

      Success (e <span class="new">:: Payload <span class="type">et</span></span>) ->
        pure . Response $ <span class="new">MkEvent</span> e

  </pre>


----

Make it compile again.
======================

.. raw:: html

  <pre>
  <span class="new">{-# LANGUAGE TypeApplications #-}</span>

  wakeUp  = importEvent <span class="new">@'<span class="type">WakeUp</span></span>
  eat     = importEvent <span class="new">@'<span class="type">Eat</span></span>
  rockOut = importEvent <span class="new">@'<span class="type">RockOut</span></span>

  eventServer :: Server EventAPI
  eventServer = serve $
    wakeUp :<|> eat :<|> rockOut

  </pre>


----

Notice that we've eliminated some boilerplate.

We no longer need to keep our Event type in sync with the payload types.

----

We can do better!
=================

Generating the API definition automatically would remove a lot more boilerplate.

The EventType now exists at the value level.

We might have a chance!

----

API changes.
============

.. raw:: html

  <pre>
  type Req  = ReqBody '[JSON] Value
  type Resp = Post    '[JSON] Response

  type EventAPI =
    "api" :>
      "event" :>
        <span class="new">Capture "event-type" EventType</span> :> Req :> Resp

  </pre>


----

Too clever for our own good.
============================

.. raw:: html

  <pre>
  importEvent :: <span class="new">EventType</span>
              -> Value
              -> ExceptT ServantErr IO Response

  importEvent <span class="new">et</span> blob =
    case fromJSON blob of
      Error err ->
        throwM err

      Success (e :: Payload <span class="type">et</span>) ->
        pure . Response $ MkEvent e

  </pre>


----

A brief interlude.
==================

On singletons.

----

Consider Unit.
==============

.. code:: haskell

  () :: ()


If you know what value you have, you know its type, and vice-versa.

----

Singletons generalize this.
===========================

We'll introduce a new type for each value we'd like to move to the type level.

Unfortunately, not the same types as provided by DataKinds.

----

.. raw:: html

  <pre class="highlight code haskell">
  {-# LANGUAGE PolyKinds  #-}
  {-# LANGUAGE TypeInType #-}

  <span class="kr">data family</span> <span class="kt">Sing</span> (<span class="type">a</span> :: <span class="kind">k</span>)

  <span class="kr">class</span> <span class="kt">SingKind</span> <span class="kind">k</span> where
    toSing   :: k -> <span class="kt">SomeSing</span> <span class="kind">k</span>
    fromSing :: <span class="kt">Sing</span> (<span class="type">a</span> :: <span class="kind">k</span>) -> k

  </pre>


----

.. raw:: html

  <pre class="highlight code haskell">
  <span class="kr">data instance</span> (<span class="kt">Sing</span> '<span class="type">True</span>)  = <span class="kt">STrue</span>
  <span class="kr">data instance</span> (<span class="kt">Sing</span> '<span class="type">False</span>) = <span class="kt">SFalse</span>


  <span class="kr">instance</span> <span class="kt">SingKind</span> <span class="kind">Bool</span> where
    toSing b = <span class="kr">case</span> b <span class="kr">of</span>
      <span class="kt">True</span>  -> <span class="kt">SomeSing STrue</span>
      <span class="kt">False</span> -> <span class="kt">SomeSing SFalse</span>

    fromSing s = <span class="kr">case</span> s <span class="kr">of</span>
      <span class="kt">STrue</span> -> <span class="kt">True</span>
      <span class="kt">False</span> -> <span class="kt">False</span>

  </pre>


----

It doesn't have to be so bad!
=============================

.. code:: haskell

  singletons [d|
    data Bool = True
              | False
    |]


----





Lots to keep in mind.
=====================

.. raw:: html

  <table>
  <thead><tr><td>Value</td><td>Type</td><td>Singleton</td><td>Singleton Type</td><td>Existential Type</td></tr></thead>
  <tr><td><pre class="highlight haskell code"><span class="kt">True</span></pre></td><td><pre class="highlight haskell code"><span class="kt">Bool</span></pre></td><td><pre class="highlight haskell code"><span class="kt">STrue</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">True</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">Bool</span></pre></td></tr>
  <tr><td><pre class="highlight haskell code"><span class="kt">False</span></pre></td><td><pre class="highlight haskell code"><span class="kt">Bool</span></pre></td><td><pre class="highlight haskell code"><span class="kt">SFalse</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">False</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">Bool</span></pre></td></tr>

  </table>


----

Not just for Bools!
===================

.. code:: haskell

  singletons [d|
    data EventType = WakeUp
                   | Eat
                   | RockOut
    |]


----

.. raw:: html

  <table>
  <thead><tr><td>Value</td><td>Type</td><td>Singleton</td><td>Singleton Type</td><td>Existential Type</td></tr></thead>
  <tr><td><pre class="highlight haskell code"><span class="kt">WakeUp</span></pre></td><td><pre class="highlight haskell code"><span class="kt">EventType</span></pre></td><td><pre class="highlight haskell code"><span class="kt">SWakeUp</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">WakeUp</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">EventType</span></pre></td></tr>
  <tr><td><pre class="highlight haskell code"><span class="kt">Eat</span></pre></td><td><pre class="highlight haskell code"><span class="kt">EventType</span></pre></td><td><pre class="highlight haskell code"><span class="kt">SEat</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">Eat</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">EventType</span></pre></td></tr>
  <tr><td><pre class="highlight haskell code"><span class="kt">RockOut</span></pre></td><td><pre class="highlight haskell code"><span class="kt">EventType</span></pre></td><td><pre class="highlight haskell code"><span class="kt">SRockOut</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">RockOut</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">EventType</span></pre></td></tr>

  </table>


----

A helper function.
==================

.. raw:: html

  <pre class="highlight code haskell">
  withSomeSing :: <span class="kt">SingKind</span> <span class="kind">k</span>
               => k
               -> (<span class="kc">forall</span> (<span class="type">a</span> :: <span class="kind">k</span>). <span class="kt">Sing</span> <span class="type">a</span> -> r)
               -> r

  </pre>


----

Back to our regularly scheduled talk.
=====================================

Armed with this knowledge, we can lift our EventType value into the type system!

----

.. raw:: html

  <pre>
  importEvent :: EventType
              -> Value
              -> ExceptT ServantErr IO Response

  importEvent etype blob =
    <span class="new">withSomeSing etype $ \ (_ :: Sing <span class="type">et</span>) -></span>
      case fromJSON blob of
        Error err ->
          throwM err

        Success (e :: Payload et) ->
          pure . Response $ MkEvent e

  </pre>


----

It doesn't work.
================

.. raw:: html

  <pre class="error">
  No instance for (FromJSON (Payload <span class="type">et</span>))
    arising from a use of `fromJSON'
    </pre>


----

Stupid compiler.
================

*We* know that `FromJSON` is total over `Payload`.

But how can we prove it?

----

If it's too hard, prove it at the term level.
=============================================

.. raw:: html

  <pre class="highlight code haskell">
  dictFromJSON :: ( <span class="kt">FromJSON</span> (<span class="kt">Payload</span> '<span class="type">WakeUp</span>)
                  , <span class="kt">FromJSON</span> (<span class="kt">Payload</span> '<span class="type">Eat</span>)
                  , <span class="kt">FromJSON</span> (<span class="kt">Payload</span> '<span class="type">RockOut</span>)
                  )
               => <span class="kt">Sing</span> (<span class="type">a</span> :: <span class="kind">EventType</span>)
               -> <span class="kt">Dict</span> (<span class="kt">FromJSON</span> (<span class="kt">Payload</span> <span class="type">a</span>))

  </pre>


A `Dict c` is a proof that we have the constraint `c`.

----

.. raw:: html

  <pre class="highlight code haskell">
  dictFromJSON :: ( <span class="kt">FromJSON</span> (<span class="kt">Payload</span> '<span class="type">WakeUp</span>)
                  , <span class="kt">FromJSON</span> (<span class="kt">Payload</span> '<span class="type">Eat</span>)
                  , <span class="kt">FromJSON</span> (<span class="kt">Payload</span> '<span class="type">RockOut</span>)
                  )
               => <span class="kt">Sing</span> (<span class="type">a</span> :: <span class="kind">EventType</span>)
               -> <span class="kt">Dict</span> (<span class="kt">FromJSON</span> (<span class="kt">Payload</span> <span class="type">a</span>))
  dictFromJSON s = <span class="kc">case</span> s <span class="kc">of</span>
    <span class="kt">SWakeUp</span>  -> <span class="kt">Dict</span>
    <span class="kt">SEat</span>     -> <span class="kt">Dict</span>
    <span class="kt">SRockOut</span> -> <span class="kt">Dict</span>

  </pre>


----

.. raw:: html

  <pre>
  importEvent :: EventType
              -> Value
              -> ExceptT ServantErr IO Response

  importEvent etype blob =
    withSomeSing etype $ \ (<span class="new">setype</span> :: Sing <span class="type">et</span>) ->
      <span class="new">case dictFromJSON setype of</span>
        <span class="new">Dict -></span>
          case fromJSON blob of
            Error err ->
              throwM err

            Success (e :: Payload <span class="type">et</span>) ->
              pure . Response $ MkEvent e

  </pre>


----

So groovy.
==========

.. raw:: html

  <pre>
  eventServer :: Server EventAPI
  eventServer = serve <span class="new">importEvent</span>

  </pre>


----

Compiler driven coding.
=======================

It is now impossible to incorrectly hook up a new EventType:

* Exhaustiveness checking of dictFromJSON ensures we made a new payload type and gave it a ToJSON instance.

* The API definitions and server handlers write themselves.

----

The other half of the problem.
==============================

We also want to serialize these new events into a single pipe for downstream consumption.

For simplicitly we'll also use JSON going downstream.

----

We know the drill.
==================

.. raw:: html

  <pre class="highlight code haskell">
  dictToJSON :: ( <span class="kt">ToJSON</span> (<span class="kt">Payload</span> '<span class="type">WakeUp</span>)
                , <span class="kt">ToJSON</span> (<span class="kt">Payload</span> '<span class="type">Eat</span>)
                , <span class="kt">ToJSON</span> (<span class="kt">Payload</span> '<span class="type">RockOut</span>)
                )
             => <span class="kt">Sing</span> (<span class="type">a</span> :: <span class="kind">EventType</span>)
             -> <span class="kt">Dict</span> (<span class="kt">ToJSON</span> (<span class="kt">Payload</span> <span class="type">a</span>))
  dictToJSON s = <span class="kc">case</span> s <span class="kc">of</span>
    <span class="kt">SWakeUp</span>  -> <span class="kt">Dict</span>
    <span class="kt">SEat</span>     -> <span class="kt">Dict</span>
    <span class="kt">SRockOut</span> -> <span class="kt">Dict</span>

  </pre>


----

We can do better!
=================

Besides the constraints under consideration, `dictToJSON` is identical to `dictFromJSON`.

----

.. raw:: html

  <pre>
  <span class="new">{-# LANGUAGE ConstraintKinds #-}</span>

  dictPayload :: ( <span class="new">c</span> (Payload '<span class="type">WakeUp</span>)
                 , <span class="new">c</span> (Payload '<span class="type">Eat</span>)
                 , <span class="new">c</span> (Payload '<span class="type">RockOut</span>)
                 )
              => Sing (<span class="type">a</span> :: <span class="kind">EventType</span>)
              -> Dict (<span class="new">c</span> (Payload <span class="type">a</span>))
  dictPayload s = case s of
    SWakeUp  -> Dict
    SEat     -> Dict
    SRockOut -> Dict

  </pre>


We can now lift *any* constraint that is total over `Payload`.

----

Let's use it to implement ToJSON over Events.

----

.. code:: haskell

  instance ToJSON Event where
    toJSON (MkEvent payload) = toJSON payload


----

It doesn't work.
================

.. raw:: html

  <pre class="error">
  No instance for (ToJSON (Payload <span class="type">et</span>))
    arising from a use of `toJSON'
    </pre>


Oh yeah. It doesn't lift automatically.

----

We need a singleton to get the Dict.
====================================

But we don't have one.

But we used to!

----

Save that singleton.
====================

.. raw:: html

  <pre>
  data Event where
    MkEvent :: <span class="new">Sing (<span class="type">et</span> :: <span class="kind">EventType</span>)</span>
            -> Payload <span class="type">et</span>
            -> Event

  </pre>


----

.. code:: haskell

  instance ToJSON Event where
    toJSON (MkEvent setype payload) =
      case dictPayload @ToJSON setype of
        Dict ->
          object [ "type"    .= fromSing setype
                 , "payload" .= payload
                 ]


We can write a similar `FromJSON` instance.

----

We're done!
===========

But what can we take away?

----

We didn't invent the Event type.
================================

In the literature, the combination of a value and a type that *depends* on that type is known as a **dependent pair**.

We can write the type of a dependent pair like this:



$$\\sum_\\text{a :: EventType} \\text{Payload}(a)$$

----

Highschool algebra.
===================

$$\\sum_\\text{a :: EventType} \\text{Payload}(a) = \\text{Payload}(a_1) + \\text{Payload}(a_2) + \\cdots + \\text{Payload}(a_n)$$

----

Look familiar?
==============

.. code:: haskell

  data Event = EventWakeUp  (Payload WakeUp)
             | EventEat     (Payload Eat)
             | EventRockOut (Payload RockOut)


This type is perfectly captured by the dependent pair.

----

More generally.
===============

$$(a, b) :: \\sum_\\text{a :: A} \\text{F}(a)$$



We can encode this directly in Haskell.

----

Namesake of the talk.
=====================

.. raw:: html

  <pre class="highlight code haskell">
  <span class="kc">data</span> <span class="kt">Some1</span> (f :: <span class="kind">k</span> -> <span class="kind">Type</span>) <span class="kc">where</span>
    <span class="kt">Some1</span> :: <span class="kt">Sing</span> (<span class="type">a</span> :: <span class="kind">k</span>) -> f <span class="type">a</span> -> <span class="kt">Some1</span> f

  </pre>


----

Specializing.
=============

.. raw:: html

  <pre>
  type Event = <span class="new">Some1 Payload</span>

  </pre>


----

But that's not all.
===================

We can generalize our `dictPayload` function as well:

.. raw:: html

  <pre class="highlight code haskell">
  <span class="kc">class</span> <span class="kt">Dict1</span> (c :: <span class="kind">output</span> -> <span class="kind">Constraint</span>)
              (f :: <span class="kind">input</span>  -> <span class="kind">output</span>) <span class="kc">where</span>
    dict1 :: <span class="kt">Sing</span> (<span class="type">a</span> :: <span class="kind">input</span>) -> <span class="kt">Dict</span> (c (f a))

  </pre>


----

It comes pre-assembled.
=======================

All of this machinery has already been built for you!

https://hackage.haskell.org/package/exinst

It also provides instances lifting Dict1 over Some1, as well as tons of other goodies.

----

Thanks for listening!
=====================

Questions?
==========

