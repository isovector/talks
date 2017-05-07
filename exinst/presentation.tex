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

Some1 Like You: Dependent Types in Haskell

- ****TODO**** lets only take in json NOT the payload
- the context
  - we want to build a data ingestion pipeline for events
  - what's an event?
    - purchase
    - weather
    - etc
  - every event has a different data schema in terms of what information it makes sense to include
  - as far as the ingestion is concerned, we don't care very much about this data
    - we want to VERIFY IT IS CORRECT
    - and then store it in a place that downstream services can retrieve
  - we expect to have >1000 different kinds of events within the next few years as we slowly take over the world
- the problem
  - we need to build a REST api to ingest this data
    - we'd like to provide meaningful errors and documentation for this api
  - we need to provide a library to downstream consumers of this data
- "maybe we can just use a big sum"
  - data Event = Purchase X Y Z | Weather L T
  - okay! cool. let's write a REST api for it
  - "api" :> "events" :> "purchase" :> Post ?
    - :<|> "api" :> "events" :> "weather" :> Post ?
  - naively maybe ? should just be our Event
  - this works! but now we don't have type safety.
    - how do we parse it?
    - try each branch and accept if any work well
    - nice! except i can send you a weather payload to a purchase endpoint
      - not the end of the world, but kinda lame
    - what's worse is WHAT IF IT DOESN'T PARSE
      - FromJSON instance isn't smart enough to know where the error occurred
    - also we have to remember to update our endpoints every time we add a new one!
  - that's pretty lame. what if we split up Event into constituent pieces
    - data PurchasePayload
      - data WeatherPayload
      - data Event = Purchase PurchasePayload | Weather WeatherPayload
    - now our endpoints can be type safe! but at the expense of we can't use the same code for each one
      - maybe we can write some `ToEvent` typeclass that lifts Payloads into Events
      - this works i guess! but it's boilerplate that's hard to automate
    - now every time we add an event we need to add its datatype, this typeclass, update our API definition, our server and our client
      - LAME
  - the other thing to consider is what will our library code look like on the other side?
    - how can people use this?
    - downstream consumers are generally going to only want to look at events of a certain type
    - Event -> Maybe (somepayload)
      - we can use another typeclass to do this
      - but notice since we're not actually using this anywhere, the compiler won't tell us if we forgot to write it
        - only downstream consumers will notice and it'll be lame for them
    - alternatively can export prisms for them to automate this bit
- so the proposal is some sort of sumtype that we maintain with all of the possibilities, and then by-hand write code to lift in (and we can automate code to get out)
  - but we still need to maybe we can somehow connectwrite by-hand API endpoints
- this idea is the right approach, but it doesn't have enough goddamn language extensions
- as it turns out, we don't really need to write this sum type!
  - we can use a GADT and datakinds
  - write an enum to describe our events
    - data EventType = Purchase | Weather
  - and if we use a data family, we can implement our payloads:
    - data family Payload (e :: EventType)
  - with a GADT
    - data Event where MkEvent :: Payload (e :: EventType) -> Event
  - oh shit we can't get it out anymore
    - push the prj typeclass into our GADT so we can reason about it
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
  -
