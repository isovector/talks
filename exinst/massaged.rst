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

----

.. raw:: html

  <pre>
  type Req  = ReqBody '[JSON] Value
  type Resp = Post    '[JSON] Response

  type EventAPI = "api" :> "event" :>
         <span class="new">Capture "event-type" EventType :> Req :> Resp</span>

  </pre>


----

.. raw:: html

  <pre>
  importEvent :: <span class="new">EventType</span>
              -> Value
              -> ExceptT ServantErr IO Response

  importEvent et blob =
    case fromJSON blob of
      Error err ->
        throwM err

      Success (e :: Payload <span class="type">et</span>) ->
        pure . Response $ MkEvent e

  </pre>


----

A brief interlude.
==================

This turns out to be a problem with a solution.

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

.. code:: haskell

  {-# LANGUAGE TemplateHaskell #-}

  $(singletons [d|
      data Bool = True
                | False
      |])


----





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

  $(singletons [d|
      data EventType = WakeUp
                     | Eat
                     | RockOut
      |])


----

.. raw:: html

  <table>
  <thead><tr><td>Value</td><td>Type</td><td>Singleton</td><td>Singleton Type</td><td>Existential Type</td></tr></thead>
  <tr><td><pre class="highlight haskell code"><span class="kt">WakeUp</span></pre></td><td><pre class="highlight haskell code"><span class="kt">EventType</span></pre></td><td><pre class="highlight haskell code"><span class="kt">SWakeUp</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">WakeUp</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">EventType</span></pre></td></tr>
  <tr><td><pre class="highlight haskell code"><span class="kt">Eat</span></pre></td><td><pre class="highlight haskell code"><span class="kt">EventType</span></pre></td><td><pre class="highlight haskell code"><span class="kt">SEat</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">Eat</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">EventType</span></pre></td></tr>
  <tr><td><pre class="highlight haskell code"><span class="kt">RockOut</span></pre></td><td><pre class="highlight haskell code"><span class="kt">EventType</span></pre></td><td><pre class="highlight haskell code"><span class="kt">SRockOut</span></pre></td><td><pre class="highlight haskell code">Sing '<span class="type">RockOut</span></pre></td><td><pre class="highlight haskell code">SomeSing <span class="kind">EventType</span></pre></td></tr>

  </table>


----

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

No instance `FromJSON` for type `et`.

----

.. code:: haskell

  dictFromJSON :: ( FromJSON (Payload 'WakeUp)
                  , FromJSON (Payload 'Eat)
                  , FromJSON (Payload 'RockOut)
                  )
               => Sing (a :: EventType)
               -> Dict (FromJSON (Payload a))


----

add constraint kind

----

.. code:: haskell

  dictFromJSON :: ( FromJSON (Payload 'WakeUp)
                  , FromJSON (Payload 'Eat)
                  , FromJSON (Payload 'RockOut)
                  )
               => Sing (a :: EventType)
               -> Dict (FromJSON (Payload a))

  dictFromJSON = \case
    SWakeUp  -> Dict
    SEat     -> Dict
    SRockOut -> Dict


----

add lambda case

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

.. raw:: html

  <pre>
  eventServer :: Server EventAPI
  eventServer = serve <span class="new">importEvent</span>

  </pre>


----

- sweet! our API implementation is done! we now get all of this for free!
  - we can add new event types to our enum
  - but we'll get a exhaustiveness error on dictFromJSON
  - which it can only be fixed if we add a data instance for the new type
  - and then everything works.
  - COMPILER DRIVEN CODING!

----

- but what about the other part of the problem?
  - we also want to serialize these things and stick them into a pipe for downstream consumers
  - for simplicity we'll encode them as json
  - assume we have some `Value -> IO ()` pipe function that sends things downstream. how can we call this function?
    - we need a ToJSON Event, duh
    - well if we want any chance of encoding it, we're going to need to know that ToJSON is total over the sum space
    - also need dictToJSON

----

.. code:: haskell

  dictToJSON :: ( ToJSON (Payload 'WakeUp)
                , ToJSON (Payload 'Eat)
                , ToJSON (Payload 'RockOut)
                )
             => Sing (a :: EventType)
             -> Dict (ToJSON (Payload a))

  dictToJSON = \case
    SWakeUp  -> Dict
    SEat     -> Dict
    SRockOut -> Dict


----

- but you'll notice that besides the constraints, this function is exactly the same implementation as dictFromJSON
  - maybe we can lift this!
    - dictEvent :: (c ...) => Sing (a :: EventType) -> Dict (c (Payload a))
  - this means that we can get a dictionary for any c (Payload a) so long as c is total over Payload a!
    - fucking sweet!

----

.. raw:: html

  <pre>
  dictPayload :: ( <span class="new">c</span> (Payload '<span class="type">WakeUp</span>)
                 , <span class="new">c</span> (Payload '<span class="type">Eat</span>)
                 , <span class="new">c</span> (Payload '<span class="type">RockOut</span>)
                 )
              => Sing (<span class="type">a</span> :: <span class="kind">EventType</span>)
              -> Dict (<span class="new">c</span> (Payload <span class="type">a</span>))

  dictPayload = \case
    SWakeUp  -> Dict
    SEat     -> Dict
    SRockOut -> Dict

  </pre>


----

.. code:: haskell

  instance ToJSON Event where
    toJSON (MkEvent payload) = toJSON payload


----

No instance `toJSON` for `Payload a`

Uh oh, we don't have a singleton to actually use to get our `dictPayload`!

Scrub lords!

----

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

    toJSON (MkEvent (setype :: Sing etype) payload) =
      case dictPayload @ToJSON setype of
        Dict ->
          object [ "type"    .= fromSing setype
                 , "payload" .= payload
                 ]


----

$$\\sum_\\text{a :: EventType} \\text{Payload}(a)$$

----

$$\\sum_\\text{a :: EventType} \\text{Payload}(a) = \\text{Payload}(a_1) + \\text{Payload}(a_2) + \\cdots + \\text{Payload}(a_n)$$

----

Look familiar?
==============

.. code:: haskell

  data Event = PayloadWakeUp  (Payload WakeUp)
             | PayloadEat     (Payload Eat)
             | PayloadRockOut (Payload RockOut)


----

More generally.
===============

$$(a, b) :: \\sum_\\text{a :: A} \\text{F}(a)$$

----

.. code:: haskell

  data Some1 (f :: k -> *) where
    Some1 :: Sing (a :: k) -> f a -> Some1 f


----

Add polykinds.

----

.. raw:: html

  <pre>
  type Event = <span class="new">Some1 Payload</span>

  </pre>


----

.. code:: haskell

  class Dict1 (c :: ok -> Constraint)
              (f :: ik -> ok) where
    dict1 :: Sing (a :: ik) -> Dict (c (f a))


