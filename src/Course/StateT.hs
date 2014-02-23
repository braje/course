{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.StateT where

import           Course.Applicative
import           Course.Apply
import           Course.Bind
import           Course.Core
import           Course.Functor
import           Course.Id
import           Course.List
import           Course.Monad
import           Course.Optional
import           Course.State
import qualified Data.Set           as S
import qualified Prelude            as P

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) f sTa =
    StateT {
      runStateT = \s -> (\(x,y) -> (f x, y)) <$> (runStateT sTa s)
    }

-- | Implement the `Apply` instance for @StateT s f@ given a @Bind f@.
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
instance Bind f => Apply (StateT s f) where
  (<*>) sTf sTa =
    StateT {
      runStateT = \s -> (( \(f,s1) -> ( (\(a,s2)-> (f a, s2) ) <$> (runStateT sTa s1) )) =<< (runStateT sTf s))
    }

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
instance Monad f => Applicative (StateT s f) where
  pure x =
    StateT {
      runStateT = \s -> pure (x,s)
    }

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
instance Monad f => Bind (StateT s f) where
  (=<<) sTf sTa =
    StateT {
      runStateT = \s -> ( \(a,s1) -> (runStateT (sTf a) s1) ) =<< (runStateT sTa s)
    }

instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f =
  StateT {
    runStateT = \s -> Id $ f s
  }

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' st s =
  runId $ runStateT st s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT stT s =
  snd <$> (runStateT stT s)

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' st s =
  runId $ execT st s

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT stT s =
  fst <$> (runStateT stT s)

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' st s =
  runId $ evalT st s

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT =
  StateT {
    runStateT = \s -> pure (s,s)
  }

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT s =
  StateT {
    runStateT = \_ -> pure ((), s)
  }

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' xs =
  eval' (filtering (\x -> getT >>= (\set -> (\_ -> S.member x set) <$> putT (S.insert x set))) xs) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
--
-- >>> distinctF $ listh [1,2,3,2,1,101,5,4]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs =
  evalT (filtering (\x ->
                     if x>100 then StateT { runStateT = \_ -> Empty }
                     else getT >>=
                      (\set ->
                        (\_ -> S.notMember x set) <$> putT (S.insert x set)))
         xs) S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) f opTa =
    OptionalT {
      runOptionalT = (\oa -> f <$> oa) <$> (runOptionalT opTa)
    }
    

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Apply f => Apply (OptionalT f) where
  (<*>) opTf opTa =
    OptionalT {
      runOptionalT = (\oF -> case oF of
                               Empty -> (\_ -> Empty)
                               Full f -> (\oa -> f <$> oa)) <$> (runOptionalT opTf) <*> (runOptionalT opTa)
    }

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure a =
    OptionalT {
      runOptionalT = pure $ Full a
    }

-- | Implement the `Bind` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Bind (OptionalT f) where
  (=<<) fOpT opTa =
    OptionalT {
      runOptionalT = (runOptionalT opTa) >>= (\opa -> case opa of
                                         Empty -> pure Empty
                                         Full a -> runOptionalT $ fOpT a
                                         )
    }

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) f (Logger logs a) = Logger logs $ f a

-- | Implement the `Apply` instance for `Logger`.
instance Apply (Logger l) where
  (<*>) (Logger lgs1 f) (Logger lgs2 a) = Logger (lgs1++lgs2) $ f a

-- | Implement the `Applicative` instance for `Logger`.
instance Applicative (Logger l) where
  pure a =
    Logger Nil a

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Bind (Logger l) where
  (=<<) f (Logger lgs a) =
    case f a of
      Logger lgs1 b -> Logger (lgs ++ lgs1) b

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l a =
  Logger (l:.Nil) a

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG xs =
  let lgevn x = if even x
                then log1 ("even number: " ++ listh (show x))
                else Logger Nil
  in
   runOptionalT $ evalT (filtering
           (\x ->
             if x>100 then StateT { runStateT = \_ -> OptionalT {
                                       runOptionalT = log1 ("aborting > 100: " ++ listh (show x)) Empty
                                   }}
             else getT >>= (\set -> StateT {
                               runStateT = \_ -> OptionalT {
                                  runOptionalT = lgevn x (Full (S.notMember x set, S.insert x set))
                                  }})) xs) S.empty
