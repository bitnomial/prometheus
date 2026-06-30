-- | A new implementation of the API, following Prometheus's standard for writing
-- client libraries: https://prometheus.io/docs/instrumenting/writing_clientlibs/
--
-- When releasing prometheus-3.0:
--   * Rename this as Prometheus
--   * Remove old 'System.Metrics.Prometheus' modules
--   * Update README/Cabal description for new API
--   * Delete this preamble
--
-- == Usage
--
-- The recommended usage is to initialize the metrics as top-level global
-- variables in the same file as the functions they're instrumenting.
--
-- Note: globally registered metrics are only registered when it's used for the
-- first time, due to laziness.
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import Prometheus.V3 qualified as Prom
-- import Prometheus.V3.Metric.Counter qualified as Counter
--
-- fooCounter :: Prom.Counter
-- fooCounter = Counter.register "foo_total" "Number of foos"
-- {-# OPAQUE fooCounter #-}
--
-- requestsCounter :: Prom.Labelled Prom.Counter
-- requestsCounter =
--   Prom.register
--     . Prom.withLabels ("method", "status") labels
--     $ Counter.new "requests_total" "Number of requests"
--   where
--     labels =
--       [ (method, status)
--       | method <- [GET, POST]
--       , status <- [200, 404, 500]
--       ]
-- {-# OPAQUE requestsCounter #-}
--
-- myHandler req = do
--   status <- run req
--
--   Counter.inc fooCounter
--   Counter.inc (Counter.labels (requestMethod req, status) requestsCounter)
--
--   -- Cache it, for a hot loop
--   c <- Counter.labels (requestMethod req, status) requestsCounter
--   Counter.inc c
--
-- main :: IO ()
-- main = do
--   Prom.serveMetrics 9090 -- Serves metrics on separate thread
-- @
--
-- Alternatively, you could initialize metrics in IO and pass it to your
-- functions as normal (e.g. Reader monad, as a function arg, etc.):
--
-- @
-- registry <- Prom.newRegistry
-- fooCounter <- Prom.registerTo registry $ Counter.new "foo_counter" ""
-- @
--
-- For advanced use-cases, you can also construct your own
-- 'Prometheus.V3.Collector.Collector' manually and register it to the registry.
module Prometheus.V3 (
    -- * Metric types
    Metric,
    Counter,
    Gauge,
    Histogram,

    -- * Labels
    Labelled,
    withLabels,
    IsLabelValue (..),

    -- * Registry
    globalRegistry,
    registerTo,
    unregisterFrom,
    register,

    -- * Serving metrics
    serveMetrics,
) where

import Prometheus.V3.Http.Serve
import Prometheus.V3.Label
import Prometheus.V3.Metric.Base
import Prometheus.V3.Metric.Counter (Counter)
import Prometheus.V3.Metric.Gauge (Gauge)
import Prometheus.V3.Metric.Histogram (Histogram)
import Prometheus.V3.Metric.Labelled
import Prometheus.V3.Registry

