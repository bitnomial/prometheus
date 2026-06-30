{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module System.Metrics.Prometheus.Metric.Counter (
    Counter,
    Counter',
    CounterSample (..),
    new,
    add,
    inc,
    sample,
    addAndSample,
    set,

    -- * Backends
    HasCounterBackend,
    CounterBackend,
    IsCounterBackend,
) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Atomics.Counter (AtomicCounter)
import qualified Data.Atomics.Counter as Atomic
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)


-- | The default Counter tracking an Int.
type Counter = Counter' Int


-- | A Counter that works for any type, but with worse performance.
newtype Counter' a = Counter {unCounter :: CounterBackend a}


newtype CounterSample a = CounterSample {unCounterSample :: a} deriving (Show)


type family CounterBackend a where
    CounterBackend Int = AtomicCounter
    CounterBackend a = IORef a


class IsCounterBackend a where
    type CounterElem a
    newCounter :: CounterElem a -> IO a
    incrCounter :: CounterElem a -> a -> IO (CounterElem a)
    writeCounter :: a -> CounterElem a -> IO ()
instance IsCounterBackend AtomicCounter where
    type CounterElem AtomicCounter = Int
    newCounter = Atomic.newCounter
    incrCounter = Atomic.incrCounter
    writeCounter = Atomic.writeCounter
instance (Num a) => IsCounterBackend (IORef a) where
    type CounterElem (IORef a) = a
    newCounter = newIORef
    incrCounter x ref = atomicModifyIORef' ref $ \a -> (a + x, a + x)
    writeCounter = writeIORef


type HasCounterBackend a =
    ( IsCounterBackend (CounterBackend a)
    , a ~ CounterElem (CounterBackend a)
    )


new ::
    (Num a, HasCounterBackend a) =>
    IO (Counter' a)
new = Counter <$> newCounter 0


addAndSample ::
    (Num a, Ord a, HasCounterBackend a) =>
    a -> Counter' a -> IO (CounterSample a)
addAndSample by
    | by >= 0 = fmap CounterSample . incrCounter by . unCounter
    | otherwise = error "must be >= 0"


add ::
    (Num a, Ord a, HasCounterBackend a) =>
    a -> Counter' a -> IO ()
add by c = addAndSample by c >> pure ()


inc ::
    (Num a, Ord a, HasCounterBackend a) =>
    Counter' a -> IO ()
inc = add 1


sample ::
    (Num a, Ord a, HasCounterBackend a) =>
    Counter' a -> IO (CounterSample a)
sample = addAndSample 0


-- | Write @i@ to the counter, if @i@ is more than the current value. This is
-- useful for when the count is maintained by a separate system (e.g. GHC's GC
-- counter).
--
-- WARNING: For multiple writers, the most recent one wins, which may not
-- preserve the increasing property. If you have stronger requirements than this,
-- please check with the maintainers.
-- See <https://github.com/bitnomial/prometheus/pull/44> for discussion.
set :: (HasCounterBackend a) => a -> Counter' a -> IO ()
set i (Counter c) = writeCounter c i
