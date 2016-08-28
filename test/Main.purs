module Test.Main where

import Prelude
import Data.Array as Array
import Control.Alt ((<|>))
import Control.Monad.Aff (later, forkAff, Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, readRef, modifyRef, newRef)
import Control.Monad.Eff.Timer (TIMER)
import Control.Observable (zip3, OBSERVABLE, Observable, observe)
import Control.Observable.Channel (close, send, asObservable, channel)
import Data.Array ((..))
import Data.Monoid (mempty, class Monoid)
import Test.Unit (timeout, suite, test, Test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

collectVals :: forall a e m. (Monoid m) => (a -> m) -> Observable a -> Aff (ref :: REF, observable :: OBSERVABLE | e) m
collectVals wrap o = makeAff \reject resolve -> do
  coll <- newRef mempty
  let collectOne a = modifyRef coll (flip append (wrap a))
      allDone = readRef coll >>= resolve
  observe collectOne reject allDone o
  pure unit

expect :: forall a e. (Eq a, Show a) => Array a -> Observable a -> Test (observable :: OBSERVABLE, ref :: REF | e)
expect m o = do
  r <- collectVals Array.singleton o
  equal m r

expectEqual :: forall a e. (Eq a, Show a) => Observable a -> Observable a -> Test (observable :: OBSERVABLE, ref :: REF | e)
expectEqual o1 o2 = do
  r1 <- collectVals Array.singleton o1
  r2 <- collectVals Array.singleton o2
  equal r1 r2

main :: forall e. Eff (avar :: AVAR, ref :: REF, console :: CONSOLE, testOutput :: TESTOUTPUT, observable :: OBSERVABLE, timer :: TIMER | e) Unit
main = runTest do

  suite "channels" do
    test "basic send/subscribe roundtrip" do
      chan ← liftEff channel
      forkAff $ later $ liftEff do
        send chan 1
        send chan 2
        send chan 3
        close chan
      timeout 100 $ expect [1,2,3] $ asObservable chan
    test "multiple subscribers" do
      chan ← liftEff channel
      forkAff $ later $ liftEff do
        send chan 1
        send chan 2
        send chan 3
        close chan
      let c1 = asObservable chan
      let c2 = asObservable chan <#> (_ + 10)
      let c3 = asObservable chan <#> (_ + 20)
      timeout 100 $ expect [[1,11,21],[2,12,22],[3,13,23]] $ zip3 (\a b c -> [a,b,c]) c1 c2 c3
