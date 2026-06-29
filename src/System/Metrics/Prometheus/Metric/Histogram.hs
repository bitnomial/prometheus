{-# LANGUAGE TupleSections #-}

module System.Metrics.Prometheus.Metric.Histogram (
    Histogram,
    HistogramSample (..),
    Buckets,
    UpperBound,
    new,
    observe,
    sample,
    observeAndSample,
    reset,
) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import qualified Data.Foldable1 as Foldable1
import Data.IORef (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
 )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


newtype Histogram = Histogram {unHistogram :: IORef HistogramSample}


type UpperBound = Double -- Inclusive upper bounds
type Buckets = Map UpperBound Double


data HistogramSample = HistogramSample
    { histBuckets :: !Buckets
    , histSum :: !Double
    , histCount :: !Int
    }
    deriving (Show)


new :: [UpperBound] -> IO Histogram
new buckets = Histogram <$> newIORef empty
  where
    empty = HistogramSample (Map.fromList $ map (,0) (read "Infinity" : buckets)) zeroSum zeroCount
    zeroSum = 0.0
    zeroCount = 0


observeAndSample :: Double -> Histogram -> IO HistogramSample
observeAndSample x = flip atomicModifyIORef' update . unHistogram
  where
    update histData = (hist' histData, hist' histData)
    hist' histData =
        histData
            { histBuckets = updateBuckets x $ histBuckets histData
            , histSum = histSum histData + x
            , histCount = histCount histData + 1
            }


observe :: Double -> Histogram -> IO ()
observe x = void . observeAndSample x


updateBuckets :: Double -> Buckets -> Buckets
updateBuckets x buckets =
    let matchingBuckets = filter (x <=) $ Map.keys buckets
        bucketKey =
            case NonEmpty.nonEmpty matchingBuckets of
                Just bs -> Foldable1.minimum bs
                Nothing -> error "Unexpectedly found zero matching buckets; at least the +Inf bucket should've been found."
     in Map.adjust (+ 1) bucketKey buckets


sample :: Histogram -> IO HistogramSample
sample = readIORef . unHistogram


-- | Reset the histogram back to its initial state: every bucket count, the
-- sum, and the observation count return to 0. The bucket upper bounds are left
-- unchanged.
--
-- This is /not/ part of the Prometheus client library specification, which
-- only defines @observe@ for histograms. It is intended for resetting metrics
-- between tests when they are defined as top-level values. On a live, scraped
-- metric a reset looks like a counter reset (e.g. a process restart) to
-- Prometheus, which its @rate@/@increase@ functions handle, so prefer
-- 'observe' there and reserve 'reset' for test isolation.
reset :: Histogram -> IO ()
reset (Histogram ref) = atomicModifyIORef' ref $ \histData ->
    ( histData
        { histBuckets = 0 <$ histBuckets histData
        , histSum = 0
        , histCount = 0
        }
    , ()
    )
