:title: Some1 Like You
:data-transition-duration: 150

:css: fonts.css
:css: presentation.css







----

:id: title

.. raw:: html

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




- now that we have unified all of this into one family, we have a chance of abstracting again
  - notice that we now have this EventType enum which exists at the term level
  - maybe we can turn our old REST apis into a capture instead of a manually unrolled enum?
  - "api" :> "events" :> Capture EventType :> Post ?
    - again what should this ? be?
    - ideally we'd like it to be (Payload e) where e is the EventType
    - but the problem is that e comes from the USER at RUNTIME
    - but the compiler wants to know what e is at compile time
  - obviously this can't work
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
- with this under our belts, we can bridge the gap into our Event GADT
  - take our capture, toSing it, and then
  - parseAsEvent
    - :: Sing (a :: EventType) -> JSON.Value -> Either ParseError Event
  - uh oh! we don't have a way of parsing EventTypes!
    - FromJSON (Payload a) =>
- what has this bought us? we don't need our own giant sum for every eventtype we'll ever want
  - the compiler can write it for us!
  - also we don't need to write our own injections into this type
  - ALSO we now have a single endpoint for all of our APIS!
    - very cool
- or do we?
  - shit. we can't prove that FromJSON (Payload a) constraint
- unfortunately there's not really any place to get this constraint from.
  - you might think we can stick it into our Event constructor, but that's too late -- we're still trying to build an Event!
  - we could prove it if we monomorphized all of our server, but then we're back to having to write glue code every time we add a new event
- are we stuck? not quite yet! i got the following trick from my brilliant coworker renzo
  - dictFromJSON :: (FromJson ...) => Sing (a :: EventType) -> Dict (FromJSON (Payload a))
  - the idea being we can use constraints on dictFromJSON to prove that we have covered the total space of FromJSON over Payload (a :: k)
    - we return a Dict which is a runtime proof that we have the constraint needed, so we can implement our server in terms of this
- withSomeSing capture $ \\(sa :: Sing (a :: EventType)) ->
  - case dictFromJSON sa of
    - Dict -> parseAsEvent sa myJSON
- sweet! our API implementation is done! we now get all of this for free!
  - we can add new event types to our enum
  - but we'll get a exhaustiveness error on dictFromJSON
  - which it can only be fixed if we add a data instance for the new type
  - and then everything works.
  - COMPILER DRIVEN CODING!
- but what about the other part of the problem?
  - we also want to serialize these things and stick them into a pipe for downstream consumers
  - for simplicity we'll encode them as json
  - assume we have some `Value -> IO ()` pipe function that sends things downstream. how can we call this function?
    - we need a ToJSON Event, duh
    - well if we want any chance of encoding it, we're going to need to know that ToJSON is total over the sum space
    - also need dictToJSON
  - but you'll notice that besides the constraints, this function is exactly the same implementation as dictFromJSON
    - maybe we can lift this!
      - dictEvent :: (c ...) => Sing (a :: EventType) -> Dict (c (Payload a))
    - this means that we can get a dictionary for any c (Payload a) so long as c is total over Payload a!
      - fucking sweet!
  - okay great! so armed with this, we might be able to write a ToJSON instance for an Event
    - as a first attempt, we can just call out to the internal type's ToJSON
    - this typechecks. but does it work?
    - let's find out. let's write the fromjson instance
      - well obviously we'll need a dictEvent :: Dict (FromJSON (Payload a))
      - but we can't get it!! because we don't have a SomeSing EventType to dispatch on to find the right instance
      - we've goofed! we've thrown away information. we don't know what type is inside our Event!
- back to the drawing board.
  - the thing is, when we constructed this thing, we (necessarily) knew what type it was
    - but we didn't store that information anywhere!
    - idiots
  - data Event where MkEvent :: Sing (a :: EventType) -> Payload a -> Event
  - this is known as a Sigma type AKA a dependent pair
    - in the literature it is
    - Sigma_{a :: EventType} Payload(a)
      - with values
      - (a :: EventType, payload :: Payload a) :: Sigma_{a :: EventType} Payload(a)
    - if you remember your highschool algebra, expanding this out algebraically is
    - Payload(a1) + Payload(a2) + Payload(a3) etc
    - aka THIS IS ACTUALLY THE EXACT SUM TYPE WE WERE BUILDING BY HAND BEFORE
  - what has this bought us? well now we can deconstruct our sigma type to get the correct EventType out, use that to dispatch dictEvent, and we can get our FromJSON instance
  - our encoding logic thus looks like this:
    - toJSON (a, payload) = (toJSON a, toJSON payload)
  - and decoding:
    - etype <- fromJSON (fst pair)
    - withSomeSing etype $ \\(s1 :: Sing (s :: EventType)) ->
      - case eventDict :: Dict (FromJSON (Payload s)) of
        - Dict -> fromJSON (snd pair)

