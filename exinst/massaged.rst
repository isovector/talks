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

.. code:: haskell

  data Event = WakeUp
             | Eat Meal
             | RockOut Song Duration

  instance FromJSON Event


----

.. code:: haskell

  type Req  = ReqBody '[JSON] Value
  type Resp = Post    '[JSON] Response

  type EventAPI = "api" :> "event" :>
         ( "wake-up"  :> Req :> Resp
      :<|> "eat"      :> Req :> Resp
      :<|> "rock-out" :> Req :> Resp
         )


----

.. code:: haskell

  importEvent :: Value -> ExceptT ServantErr IO Response
  importEvent blob =
    case fromJSON blob of
      Error   err -> throwError err
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

.. code:: haskell

  data PayloadWakeUp  = WakeUp
  data PayloadEat     = Eat Meal
  data PayloadRockOut = RockOut Song Duration

  instance FromJSON PayloadWakeUp
  instance FromJSON PayloadEat
  instance FromJSON PayloadRockOut


----

.. raw:: html

  <pre>
  data Event = PayloadWakeUp  <span class="new">PayloadWakeUp</span>
             | PayloadEat     <span class="new">PayloadEat</span>
             | PayloadRockOut <span class="new">PayloadRockOut</span>

  <span class="new">makePrisms ''Event</span>

  </pre>


----

.. raw:: html

  <pre>
  importEvent :: <span class="new">FromJSON e</span>
              <span class="new">=> Prism' Event e</span>
              -> Value
              -> ExceptT ServantErr IO Response
  importEvent <span class="new">prism</span> blob =
    case fromJSON blob of
      Error   err -> throwError err
      Success e   -> pure . Response $ <span class="new">review prism</span> e

  </pre>


----

.. raw:: html

  <pre>
  {-# LANGUAGE RankNTypes #-}

  </pre>


----

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

.. code:: haskell

  data EventType = WakeUp | Eat | RockOut


----

.. code:: haskell

  data family Payload (e :: EventType)


----

.. raw:: html

  <pre>
  <span class="new">{-# LANGUAGE DataKinds    #-}</span>
  {-# LANGUAGE RankNTypes   #-}
  <span class="new">{-# LANGUAGE TypeFamilies #-}</span>

  </pre>


----

.. raw:: html

  <pre>
  data <span class="new">instance Payload 'WakeUp</span>  = WakeUp
  data <span class="new">instance Payload 'Eat</span>     = Eat Meal
  data <span class="new">instance Payload 'RockOut</span> = RockOut Song Duration

  instance FromJSON (Payload 'WakeUp)
  instance FromJSON (Payload 'Eat)
  instance FromJSON (Payload 'RockOut)

  </pre>


----

.. code:: haskell

  data Event where
    MkEvent :: Payload (et :: EventType) -> Event


----

.. raw:: html

  <pre>
  importEvent :: <span class="new">forall (et :: EventType)</span>
               . FromJSON (<span class="new">Payload</span> et)
              -> <span class="new">Proxy et</span>
              -> Value
              -> ExceptT ServantErr IO Response

  importEvent <span class="new">_</span> blob =
    case fromJSON blob of
      Error err ->
        throwError err

      Success (e <span class="new">:: Payload et</span>) ->
        pure . Response $ <span class="new">MkEvent</span> e

  </pre>


----

.. raw:: html

  <pre>
  {-# LANGUAGE DataKinds           #-}
  <span class="new">{-# LANGUAGE KindSigs            #-}</span>
  {-# LANGUAGE RankNTypes          #-}
  <span class="new">{-# LANGUAGE ScopedTypeVariables #-}</span>
  {-# LANGUAGE TypeFamilies        #-}

  </pre>



----

.. raw:: html

  <pre>
  wakeUp  = importEvent <span class="new">(Proxy @'WakeUp)</span>
  eat     = importEvent <span class="new">(Proxy @'Eat)</span>
  rockOut = importEvent <span class="new">(Proxy @'RockOut)</span>

  eventServer :: Server EventAPI
  eventServer = serve $
    wakeUp :<|> eat :<|> rockOut

  </pre>


----

.. raw:: html

  <pre>
  {-# LANGUAGE DataKinds           #-}
  {-# LANGUAGE KindSigs            #-}
  {-# LANGUAGE RankNTypes          #-}
  {-# LANGUAGE ScopedTypeVariables #-}
  <span class="new">{-# LANGUAGE TypeApplications    #-}</span>
  {-# LANGUAGE TypeFamilies        #-}

  </pre>


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

  importEvent <span class="wat">et</span> blob =
    case fromJSON blob of
      Error err ->
        throwError err

      Success (e :: Payload <span class="wat">et</span>) ->
        pure . Response $ MkEvent e

  </pre>


----

- OR CAN IT
  - introducing singletons
  - singletons allow us to bridge the gap between types and terms
  - think about the type ()
    - if you know what type it is, you know the value of it
    - if you have a value of (), you know what type it is
    - we have an injective function from these terms to types
      - because it's injective we can go both directions
  - the problem is that dealing with these things at the term level is hard -- since they all have different types
    - we can existentalize over them to let us fit them into something at the term level
    - data SomeSing k where SomeSing :: (Sing a :: k) -> SomeSing k
  - singletons also give us the ability to lift in and out of singletons
    - toSing :: SingKind k => k -> SomeSing k
    - fromSing :: Sing (a :: k) -> k

----

.. raw:: html

  <pre>
  importEvent :: EventType
              -> Value
              -> ExceptT ServantErr IO Response

  importEvent etype blob =
    <span class="new">withSomeSing etype $ \ (_ :: Sing et) -></span>
      case fromJSON blob of
        Error err ->
          throwError err

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
    withSomeSing etype $ \ (<span class="new">setype</span> :: Sing et) ->
      <span class="new">case dictFromJSON setype of</span>
        <span class="new">Dict -></span>
          case fromJSON blob of
            Error err ->
              throwError err

            Success (e :: Payload et) ->
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
             -> Dict (FromJSON (Payload a))

  dictFromJSON = \case
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
  dictPayload :: ( <span class="new">c</span> (Payload 'WakeUp)
                 , <span class="new">c</span> (Payload 'Eat)
                 , <span class="new">c</span> (Payload 'RockOut)
                 )
              => Sing (a :: EventType)
              -> Dict (<span class="new">c</span> (Payload a))

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
    MkEvent :: <span class="new">Sing (et :: EventType)</span>
            -> Payload et
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

- this is known as a Sigma type AKA a dependent pair
  - in the literature it is
  - Sigma_{a :: EventType} Payload(a)
    - with values
    - (a :: EventType, payload :: Payload a) :: Sigma_{a :: EventType} Payload(a)
  - if you remember your highschool algebra, expanding this out algebraically is
  - Payload(a1) + Payload(a2) + Payload(a3) etc
  - aka THIS IS ACTUALLY THE EXACT SUM TYPE WE WERE BUILDING BY HAND BEFORE

----

$$\\sum_\\text{a :: EventType} \\text{Payload}(a)$$

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


