module Control.Observable.Channel
  ( channel
  , send
  , throw
  , close
  , asObservable
  , Channel
  ) where

import Prelude
import Data.CatList as List
import Data.StrMap as Map
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF, Ref, readRef, modifyRef, modifyRef', newRef)
import Control.Observable (Observer, OBSERVABLE, free, observable, Observable)
import Data.Foldable (class Foldable, foldMap, sequence_, foldr, foldl)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))



-- | A set like data structure for things without equality, such as the
-- | `Observer`s we need to keep track of for `Channel`s.
-- |
-- | It can cleanly remove items from the set, given the `RefSetEntry`
-- | that was returned from their insertion.

data RefSet a = RefSet Int (StrMap a)

data RefSetEntry a = RefSetEntry Int a

type RefSetResult a = Tuple (RefSet a) (RefSetEntry a)

empty :: ∀ a. RefSet a
empty = RefSet 0 Map.empty

insert :: ∀ a. a → RefSet a → RefSetResult a
insert v (RefSet next store) =
  RefSet (next + 1) (Map.insert (show next) v store) /\ RefSetEntry next v

delete :: ∀ a. RefSetEntry a → RefSet a → RefSet a
delete (RefSetEntry key _) (RefSet next store) =
  RefSet next (Map.delete (show key) store)

instance foldableRefSet :: Foldable RefSet where
  foldl f i (RefSet _ store) = foldl f i (Map.values store)
  foldr f i (RefSet _ store) = foldr f i (Map.values store)
  foldMap f (RefSet _ store) = foldMap f (Map.values store)





-- | A `Channel` wraps an `Observable` into which you can feed values
-- | using the `send` function.
data Channel e a = Channel (Observable a) (Ref (RefSet (Observer e a)))

-- | Create a `Channel`.
channel :: ∀ e a. Eff (observable :: OBSERVABLE, ref :: REF | e) (Channel (ref :: REF | e) a)
channel = do
  sinks ← newRef empty
  obs ← observable \sink → do
    target ← modifyRef' sinks (append sink)
    free [{unsubscribe: modifyRef sinks (delete target)}]
  pure $ Channel obs sinks
    where append sink set = case insert sink set of
            (set' /\ target') → {state: set', value: target'}

mapChannel :: ∀ e a. Channel (ref :: REF | e) a → (Observer (ref :: REF | e) a → Eff (observable :: OBSERVABLE, ref :: REF | e) Unit) → Eff (observable :: OBSERVABLE, ref :: REF | e) Unit
mapChannel (Channel _ sinks') action = do
  sinks ← readRef sinks'
  sequence_ $ foldMap sendToSink sinks
    where sendToSink sink =
            single $ action sink
          single v = List.cons v List.CatNil

-- | Send a value to a `Channel`.
-- |
-- | `send` will pass the value on to any `Observer`s subscribed at
-- | the time of the call. If there are no subscribers, the value is
-- | discarded.
send :: ∀ e a. Channel (ref :: REF | e) a → a → Eff (observable :: OBSERVABLE, ref :: REF | e) Unit
send chan value = mapChannel chan \sink → sink.next value

-- | Send an error to a `Channel`.
throw :: ∀ e a. Channel (ref :: REF | e) a → Error → Eff (observable :: OBSERVABLE, ref :: REF | e) Unit
throw chan err = mapChannel chan \sink → sink.error err

-- | Close a `Channel`.
close :: ∀ e a. Channel (ref :: REF | e) a → Eff (observable :: OBSERVABLE, ref :: REF | e) Unit
close chan = mapChannel chan \sink → sink.complete

-- | Get the `Observable` for the `Channel`, which you can subscribe
-- | to in order to receive the values sent to it using `send`.
asObservable :: ∀ e a. Channel e a → Observable a
asObservable (Channel o _) = o
