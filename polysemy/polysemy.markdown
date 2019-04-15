:title: Some1 Like You
:data-transition-duration: 150

:css: fonts.css
:css: presentation.css


\newenvironment{hs}{.. code:: haskell
}{}
\newenvironment{raw}{.. raw:: html

  <pre>}{
  </pre>
}
\newcommand{\$}{\begin{verbatim}$\end{verbatim}}
\newcommand{\type}[1]{<span class="type">#1</span>}

----

:id: title

.. raw:: html

  <script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>

  <h1>Some1 Like You</h1>
  <h2>Dependent Pairs in Haskell</h2>
  <h3>A talk by <span>Sandy Maguire</span></h3>
  <h4>reasonablypolymorphic.com</h4>

----

Slides available.
=================

\begin{raw}
  <h3>reasonablypolymorphic.com/some1-like-you</h3>
\end{raw}

----


\begin{hs}
data Teletype k
  = Pure k
  | WriteLine String (Teletype k)
  | ReadLine (String -> Teletype k)
  deriving (Functor, Applicative) via WrappedMonad Teletype
\end{hs}

----

\begin{hs}
instance Monad Teletype where
  return = Pure
  Pure          k >>= f = f k
  WriteLine msg k >>= f = WriteLine msg $ k >>= f
  ReadLine      k >>= f = ReadLine $ \str -> k str >>= f
\end{hs}

----

\begin{hs}
echo :: Teletype ()
echo = ReadLine
     \$ \msg -> WriteLine msg
     \$ Pure ()
\end{hs}

----

\begin{hs}
echo :: Teletype ()
echo = do
  msg <- ReadLine pure
  WriteLine msg $ pure ()
\end{hs}

----

\begin{hs}
runTeletypeInIO :: Teletype a -> IO a
runTeletypeInIO (Pure a) = pure a
runTeletypeInIO (WriteLine msg k) = do
  putStrLn msg
  runTeletypeInIO k
runTeletypeInIO (ReadLine k) =  do
  msg <- getLine
  runTeletypeInIO $ k msg
\end{hs}

----

\begin{hs}
runTeletypePurely :: [String] -> Teletype a -> ([String], a)
runTeletypePurely _ (Pure a) = ([], a)
runTeletypePurely ls (WriteLine msg k) =
  let (rs, a) = runTeletypePurely ls k
   in (msg : rs, a)
runTeletypePurely []       (ReadLine k) =  runTeletypePurely [] $ k ""
runTeletypePurely (l : ls) (ReadLine k) =  runTeletypePurely ls $ k l
\end{hs}

----

\begin{hs}
data Free f k
  = Pure k
  | Impure (f (Free f k))
  deriving (Functor, Applicative) via WrappedMonad (Free f)
\end{hs}

----

\begin{hs}
instance Functor f => Monad (Free f) where
  return = Pure
  Pure k   >>= f = f k
  Impure z >>= f = Impure $ fmap (\x -> x >>= f) z
\end{hs}

----

\begin{hs}
data Teletype a
  = WriteLine String a
  | ReadLine (String -> a)
  deriving Functor
\end{hs}

----

\begin{hs}
writeLine :: String -> Free Teletype' ()
writeLine msg = Impure $ WriteLine' msg $ pure ()

readLine :: Free Teletype' String
readLine = Impure $ ReadLine' pure
\end{hs}

----

-- TODO(sandy): remove the trailing ticks in this file

\begin{hs}
echo :: Free Teletype' ()
echo = do
  msg <- readLine
  writeLine msg
\end{hs}

----

\begin{hs}
runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
runFree _ (Pure' a)  = pure a
runFree f (Impure k) = f k >>= runFree f
\end{hs}

----

\begin{hs}
runTeletypeInIO' :: Free Teletype' a -> IO a
runTeletypeInIO' = runFree $ \case
  WriteLine' msg k -> do
    putStrLn msg
    pure k
  ReadLine' k -> do
    msg <- getLine
    pure $ k msg
\end{hs}

----

\begin{hs}
data Bell k
  = RingBell k
  deriving Functor
\end{hs}

----

\begin{hs}
data Sum f g a
  = L (f a)
  | R (g a)
  deriving Functor

type TeletypeWithBell = Sum Teletype Bell
\end{hs}

----

\begin{hs}
writeLine' :: String -> Free TeletypeWithBell ()
writeLine' msg = Impure $ L $ WriteLine' msg $ pure ()

readLine' :: Free TeletypeWithBell String
readLine' = Impure $ L $ ReadLine' pure

ringBell :: Free TeletypeWithBell ()
ringBell = Impure $ R $ RingBell $ pure ()
\end{hs}

----

\begin{hs}
ringItSingIt :: Free TeletypeWithBell String
ringItSingIt = do
  msg <- readLine'
  when (msg == "ring the bell!") ringBell
  pure msg
\end{hs}

----

\begin{hs}
data Union r a

class Member f r where
  inj  :: f a       -> Union r a
  proj :: Union r a -> Maybe (f a)
\end{hs}

----

\begin{hs}
writeLine'' :: Member Teletype' r => String -> Free (Union r) ()
writeLine'' msg = Impure $ inj $ WriteLine' msg $ pure ()

readLine'' :: Member Teletype' r => Free (Union r) String
readLine'' = Impure $ inj $ ReadLine' pure

ringBell'' :: Member Bell r => Free (Union r) ()
ringBell'' = Impure $ inj $ RingBell $ pure ()
\end{hs}

----

Various Extensions

----


\begin{hs}
runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
\end{hs}

What if we just GADT'd it?

\begin{hs}
data Teletype'' a where
  WriteLine'' :: String -> Teletype'' ()
  ReadLine''  :: Teletype'' String

data Bell'' a where
  RingBell'' :: Bell'' ()
\end{hs}

No more \ty{Functor}s!

----

\begin{hs}
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
\end{hs}

----

\begin{hs}
runReader :: Monad m => ReaderT r m a -> r -> m a

runFree
    :: Monad m
    => (∀ x. f x -> m x)
    -> Free f a
    -> m a
\end{hs}

This thing is a \ty{ReaderT} in disguise!

----


\begin{hs}
newtype Freer r a = Freer
  { unFreer
        :: ∀ m. Monad m
        => (∀ x. Union r x -> m x)
        -> m a
  }
  deriving (Functor, Applicative) via WrappedMonad (Freer r)

runFreer :: Monad m => (∀ x. Union r x -> m x) -> Freer r a -> m a
runFreer nt m = unFreer m nt
\end{hs}

----

\begin{hs}
instance Monad (Freer f) where
  return a = Freer $ \nt -> pure a
  m >>= f  = Freer $ \nt -> do
    a <- runFreer m nt
    runFreer (f a) nt

instance (Monad m) => Monad (ReaderT r m) where
  return a = ReaderT $ \r -> pure a
  m >>= k  = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (k a) r
\end{hs}

----

\begin{hs}
liftFreer :: Member f r => f a -> Freer r a
liftFreer fa = Freer $ \nt -> nt $ inj fa
\end{hs}

----

\begin{hs}
writeLine''' :: Member Teletype'' r => String -> Freer r ()
writeLine''' msg = liftFreer $ WriteLine'' msg

readLine''' :: Member Teletype'' r => Freer r String
readLine''' = liftFreer ReadLine''

ringBell''' :: Member Bell'' r => Freer r ()
ringBell''' = liftFreer RingBell''
\end{hs}

----

What the heck is going on?

Now any time our free monad wants to use an action, it immediately runs it in
the final monad.

---

\begin{hs}
echo' :: Member Teletype r => Freer r ()
echo' = do
  msg <- readLine
  writeLine msg
\end{hs}

----

\begin{hs}
echo :: Member Teletype r => Freer r ()
echo = do
  msg <- readLine
  writeLine msg

echoIO :: IO ()
echoIO = runFreer runTeletypeInIO echo

echoIO :: IO ()
echoIO = runFreer runTeletypeInIO $ do
  msg <- readLine
  writeLine msg

echoIO :: IO ()
echoIO = do
  msg <- runTeletypeInIO readLine
  runTeletypeInIO $ writeLine msg

echoIO :: IO ()
echoIO = do
  msg <- case ReadLine of
           ReadLine      -> getLine
           WriteLine msg -> putStrLn msg
  case WriteLine msg of
    ReadLine -> getLine
    WriteLine msg -> putStrLn msg

echoIO :: IO ()
echoIO = do
  msg <- case ReadLine of
           ReadLine      -> getLine
           -- WriteLine msg -> putStrLn msg
  case WriteLine msg of
    -- ReadLine -> getLine
    WriteLine msg -> putStrLn msg

echoIO :: IO ()
echoIO = do
  msg <- case ReadLine of
           ReadLine      -> getLine
  case WriteLine msg of
    WriteLine msg -> putStrLn msg

echoIO :: IO ()
echoIO = do
  msg <- getLine
  putStrLn msg
\end{hs}

So free!

----

Shoutouts to Li-Yao Xia for the final encoding
And to Ollie Charles for pointing out that this thing is just a ReaderT

----

Let's rewind.

----

\begin{hs}
throw
    :: Member (Error e) r
    => e
    -> Semantic r a

catch
    :: Member (Error e) r
    => Semantic r a
    -> (e -> Semantic r a)
    -> Semantic r a
\end{hs}

----

\begin{hs}
data Error e k
  = Throw e
  | ∀ x. Catch (???)
               (e -> ???)
               (x -> k)
\end{hs}

----

\begin{hs}
data Error e m k
  = Throw e
  | ∀ x. Catch (m x)
               (e -> m x)
               (x -> k)
  deriving Functor
\end{hs}

----

\begin{hs}
data Teletype' m a
  = WriteLine' String a
  | ReadLine' (String -> a)
  deriving Functor
\end{hs}

----

