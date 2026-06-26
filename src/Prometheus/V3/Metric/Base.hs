{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Metric.Base (
    MetricName,
    Description,
    Metric (..),
    IsMetric (..),
    MetricType (..),

    -- * Helpers
    withDuration,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Prometheus.V3.Label (LabelName)
import Prometheus.V3.MetricName (MetricName)
import Prometheus.V3.Sample (Sample)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)


type Description = Text


data Metric a = Metric
    { name :: MetricName
    , description :: Description
    , initialize :: IO a
    }


data MetricType
    = MetricTypeCounter
    | MetricTypeGauge
    | MetricTypeSummary
    | MetricTypeHistogram
    | MetricTypeUntyped
    deriving stock (Show, Eq, Enum, Bounded)


class IsMetric a where
    {-# MINIMAL getMetricType, getMetricSamples #-}
    getMetricType :: Proxy a -> MetricType
    getMetricSamples :: a -> IO [Sample]


    isValidMetricLabel :: Proxy a -> LabelName -> Bool
    isValidMetricLabel _ _ = True


{----- Helpers -----}

withDuration :: (MonadUnliftIO m) => (Double -> m ()) -> m a -> m a
withDuration f action = bracket (liftIO getCurrentTime) finalize (\_ -> action)
  where
    finalize start = do
        end <- liftIO getCurrentTime
        f (realToFrac $ end `diffUTCTime` start)
