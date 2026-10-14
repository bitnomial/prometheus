{-# LANGUAGE OverloadedRecordDot #-}
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

import Control.Monad (when)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.Vector.Unboxed as Vector.Unboxed
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as IOVector
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)


newtype Histogram = Histogram (MVar HistogramState)


type UpperBound = Double -- Inclusive upper bounds
type Buckets = Map UpperBound Double


data HistogramSample = HistogramSample
    { histBuckets :: !Buckets
    -- ^ Cumulative counts
    , histSum :: !Double
    , histCount :: !Int
    }
    deriving (Show)


data HistogramState = HistogramState
    { histStateBuckets :: !(IOVector (UpperBound, Double))
    -- ^ Contains cumulative counts
    --
    -- Invariants:
    --   * Upper bounds are sorted low to high
    --   * Last element is Infinity
    , histStateSum :: !Double
    , histStateCount :: !Int
    }


new :: [UpperBound] -> IO Histogram
new buckets = do
    bucketsVec <-
        Vector.Unboxed.thaw . Vector.Unboxed.fromList $
            [ (bucket, 0)
            | bucket <- sort $ buckets <> [read "Infinity"]
            ]
    Histogram <$> newMVar (mkEmpty bucketsVec)
  where
    mkEmpty bucketsVec =
        HistogramState
            { histStateBuckets = bucketsVec
            , histStateSum = 0
            , histStateCount = 0
            }


observeAndSample :: Double -> Histogram -> IO HistogramSample
observeAndSample x (Histogram var) =
    modifyMVar var $ \state -> do
        state' <- observeAndSampleState x state
        sample_ <- toSample state'
        pure (state', sample_)


observe :: Double -> Histogram -> IO ()
observe x (Histogram var) = modifyMVar_ var (observeAndSampleState x)


sample :: Histogram -> IO HistogramSample
sample (Histogram var) =
    -- Do NOT use readMVar; make sure no one else is modifying the bucket
    -- counts when we're sampling
    withMVar var toSample


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
reset (Histogram var) = modifyMVar_ var $ \state -> do
    IOVector.iforM_ state.histStateBuckets $ \i (bucket, _) -> do
        IOVector.unsafeWrite state.histStateBuckets i (bucket, 0)
    pure state{histStateSum = 0, histStateCount = 0}


observeAndSampleState :: Double -> HistogramState -> IO HistogramState
observeAndSampleState x state = do
    IOVector.iforM_ state.histStateBuckets $ \i (bucket, count) -> do
        when (x <= bucket) $ do
            IOVector.unsafeWrite state.histStateBuckets i (bucket, count + 1)
    pure
        state
            { histStateSum = state.histStateSum + x
            , histStateCount = state.histStateCount + 1
            }


toSample :: HistogramState -> IO HistogramSample
toSample state = do
    buckets <- toList state.histStateBuckets
    pure
        HistogramSample
            { histBuckets = Map.fromAscList buckets
            , histSum = state.histStateSum
            , histCount = state.histStateCount
            }
  where
    toList v = go (IOVector.length v - 1) []
      where
        go i acc
            | i < 0 = pure acc
            | otherwise = do
                x <- IOVector.unsafeRead v i
                go (i - 1) (x : acc)
