module System.Metrics.Prometheus.Metric.Counter (
    Counter,
    CounterSample (..),
    new,
    add,
    inc,
    sample,
    addAndSample,
    set,
) where

import Control.Monad (when)
import Data.Atomics.Counter (AtomicCounter, incrCounter, newCounter, writeCounter)


newtype Counter = Counter {unCounter :: AtomicCounter}
newtype CounterSample = CounterSample {unCounterSample :: Int} deriving Show


new :: IO Counter
new = Counter <$> newCounter 0


addAndSample :: Int -> Counter -> IO CounterSample
addAndSample by
    | by >= 0 = fmap CounterSample . incrCounter by . unCounter
    | otherwise = error "must be >= 0"


add :: Int -> Counter -> IO ()
add by c = addAndSample by c >> pure ()


inc :: Counter -> IO ()
inc = add 1


sample :: Counter -> IO CounterSample
sample = addAndSample 0


-- | Write @i@ to the counter, if @i@ is more than the current value. This is
-- useful for when the count is maintained by a separate system (e.g. GHC's GC
-- counter).
--
-- WARNING: For multiple writers, the most recent one wins, which may not
-- preserve the increasing property. If you have stronger requirements than this,
-- please check with the maintainers. 
-- See <https://github.com/bitnomial/prometheus/pull/44> for discussion.
set :: Int -> Counter -> IO ()
set i (Counter c) = writeCounter c i
