{-# LANGUAGE OverloadedStrings #-}

-- |
-- GHC RTS metrics exported through the 'prometheus' registry.
--
-- 'registerGHCStats' registers metric handles in the current 'RegistryT' and
-- returns an 'IO' action that snapshots 'GHC.Stats.getRTSStats' and writes
-- each field to its handle. Compose the updater with the registry sample so
-- every scrape sees fresh values:
--
-- @
-- (updater, regSample) <- runRegistryT $ (,) \<$\> registerGHCStats \<*\> sample
-- serveMetrics port path (updater >> regSample)
-- @
--
-- Requires the executable to be run with @+RTS -T@; otherwise
-- 'getRTSStatsEnabled' returns 'False', the updater is a no-op, and every
-- metric reports @0@.
--
-- Time fields are exposed in nanoseconds (the unit @GHC.Stats@ uses); divide
-- by @1e9@ in PromQL for seconds. The 'Counter' type in this @prometheus@
-- fork is @Int@-backed, so fractional seconds cannot be represented
-- losslessly through it.
module System.Metrics.Prometheus.GHCStats (
    registerGHCStats,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import GHC.Stats (
    GCDetails (..),
    RTSStats (..),
    getRTSStats,
    getRTSStatsEnabled,
 )
import System.Metrics.Prometheus.Concurrent.RegistryT (
    RegistryT,
    registerCounter,
    registerGauge,
 )
import System.Metrics.Prometheus.Metric.Counter (Counter)
import System.Metrics.Prometheus.Metric.Counter qualified as Counter
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import System.Metrics.Prometheus.Metric.Gauge qualified as Gauge


registerGHCStats :: MonadIO m => RegistryT m (IO ())
registerGHCStats = do
    cGcs <- counter "ghc_gcs_total"
    cMajorGcs <- counter "ghc_major_gcs_total"
    cAllocated <- counter "ghc_allocated_bytes_total"
    cCopied <- counter "ghc_copied_bytes_total"
    cParCopied <- counter "ghc_par_copied_bytes_total"
    cCumLive <- counter "ghc_cumulative_live_bytes_total"
    cCumParBalanced <- counter "ghc_cumulative_par_balanced_copied_bytes_total"
    cInitCpu <- counter "ghc_init_cpu_ns_total"
    cInitElapsed <- counter "ghc_init_elapsed_ns_total"
    cMutCpu <- counter "ghc_mutator_cpu_ns_total"
    cMutElapsed <- counter "ghc_mutator_elapsed_ns_total"
    cGcCpu <- counter "ghc_gc_cpu_ns_total"
    cGcElapsed <- counter "ghc_gc_elapsed_ns_total"
    cCpu <- counter "ghc_cpu_ns_total"
    cElapsed <- counter "ghc_elapsed_ns_total"
    cNonmovingCpu <- counter "ghc_nonmoving_gc_cpu_ns_total"
    cNonmovingElapsed <- counter "ghc_nonmoving_gc_elapsed_ns_total"
    cNonmovingSyncCpu <- counter "ghc_nonmoving_gc_sync_cpu_ns_total"
    cNonmovingSyncElapsed <- counter "ghc_nonmoving_gc_sync_elapsed_ns_total"

    gMaxLive <- gauge "ghc_max_live_bytes"
    gMaxLargeObjects <- gauge "ghc_max_large_objects_bytes"
    gMaxCompact <- gauge "ghc_max_compact_bytes"
    gMaxSlop <- gauge "ghc_max_slop_bytes"
    gMaxMemInUse <- gauge "ghc_max_mem_in_use_bytes"
    gNonmovingSyncMaxElapsed <- gauge "ghc_nonmoving_gc_sync_max_elapsed_ns"
    gNonmovingMaxElapsed <- gauge "ghc_nonmoving_gc_max_elapsed_ns"

    gDetailsGen <- gauge "ghc_gcdetails_gen"
    gDetailsThreads <- gauge "ghc_gcdetails_threads"
    gDetailsAllocated <- gauge "ghc_gcdetails_allocated_bytes"
    gDetailsLive <- gauge "ghc_gcdetails_live_bytes"
    gDetailsLargeObjects <- gauge "ghc_gcdetails_large_objects_bytes"
    gDetailsCompact <- gauge "ghc_gcdetails_compact_bytes"
    gDetailsSlop <- gauge "ghc_gcdetails_slop_bytes"
    gDetailsMemInUse <- gauge "ghc_gcdetails_mem_in_use_bytes"
    gDetailsCopied <- gauge "ghc_gcdetails_copied_bytes"
    gDetailsParMaxCopied <- gauge "ghc_gcdetails_par_max_copied_bytes"
    gDetailsParBalancedCopied <- gauge "ghc_gcdetails_par_balanced_copied_bytes"
    gDetailsSyncElapsed <- gauge "ghc_gcdetails_sync_elapsed_ns"
    gDetailsCpu <- gauge "ghc_gcdetails_cpu_ns"
    gDetailsElapsed <- gauge "ghc_gcdetails_elapsed_ns"
    gDetailsNonmovingSyncCpu <- gauge "ghc_gcdetails_nonmoving_gc_sync_cpu_ns"
    gDetailsNonmovingSyncElapsed <- gauge "ghc_gcdetails_nonmoving_gc_sync_elapsed_ns"

    pure $ do
        enabled <- getRTSStatsEnabled
        when enabled $ do
            stats <- getRTSStats
            setCounterFrom stats gcs cGcs
            setCounterFrom stats major_gcs cMajorGcs
            setCounterFrom stats allocated_bytes cAllocated
            setCounterFrom stats copied_bytes cCopied
            setCounterFrom stats par_copied_bytes cParCopied
            setCounterFrom stats cumulative_live_bytes cCumLive
            setCounterFrom stats cumulative_par_balanced_copied_bytes cCumParBalanced
            setCounterFrom stats init_cpu_ns cInitCpu
            setCounterFrom stats init_elapsed_ns cInitElapsed
            setCounterFrom stats mutator_cpu_ns cMutCpu
            setCounterFrom stats mutator_elapsed_ns cMutElapsed
            setCounterFrom stats gc_cpu_ns cGcCpu
            setCounterFrom stats gc_elapsed_ns cGcElapsed
            setCounterFrom stats cpu_ns cCpu
            setCounterFrom stats elapsed_ns cElapsed
            setCounterFrom stats nonmoving_gc_cpu_ns cNonmovingCpu
            setCounterFrom stats nonmoving_gc_elapsed_ns cNonmovingElapsed
            setCounterFrom stats nonmoving_gc_sync_cpu_ns cNonmovingSyncCpu
            setCounterFrom stats nonmoving_gc_sync_elapsed_ns cNonmovingSyncElapsed

            setGaugeFrom stats max_live_bytes gMaxLive
            setGaugeFrom stats max_large_objects_bytes gMaxLargeObjects
            setGaugeFrom stats max_compact_bytes gMaxCompact
            setGaugeFrom stats max_slop_bytes gMaxSlop
            setGaugeFrom stats max_mem_in_use_bytes gMaxMemInUse
            setGaugeFrom stats nonmoving_gc_sync_max_elapsed_ns gNonmovingSyncMaxElapsed
            setGaugeFrom stats nonmoving_gc_max_elapsed_ns gNonmovingMaxElapsed

            let details = gc stats
            setGaugeFrom details gcdetails_gen gDetailsGen
            setGaugeFrom details gcdetails_threads gDetailsThreads
            setGaugeFrom details gcdetails_allocated_bytes gDetailsAllocated
            setGaugeFrom details gcdetails_live_bytes gDetailsLive
            setGaugeFrom details gcdetails_large_objects_bytes gDetailsLargeObjects
            setGaugeFrom details gcdetails_compact_bytes gDetailsCompact
            setGaugeFrom details gcdetails_slop_bytes gDetailsSlop
            setGaugeFrom details gcdetails_mem_in_use_bytes gDetailsMemInUse
            setGaugeFrom details gcdetails_copied_bytes gDetailsCopied
            setGaugeFrom details gcdetails_par_max_copied_bytes gDetailsParMaxCopied
            setGaugeFrom details gcdetails_par_balanced_copied_bytes gDetailsParBalancedCopied
            setGaugeFrom details gcdetails_sync_elapsed_ns gDetailsSyncElapsed
            setGaugeFrom details gcdetails_cpu_ns gDetailsCpu
            setGaugeFrom details gcdetails_elapsed_ns gDetailsElapsed
            setGaugeFrom details gcdetails_nonmoving_gc_sync_cpu_ns gDetailsNonmovingSyncCpu
            setGaugeFrom details gcdetails_nonmoving_gc_sync_elapsed_ns gDetailsNonmovingSyncElapsed
  where
    counter n = registerCounter n mempty
    gauge n = registerGauge n mempty


setCounterFrom :: Integral a => s -> (s -> a) -> Counter -> IO ()
setCounterFrom s f = Counter.set (fromIntegral (f s))


setGaugeFrom :: Integral a => s -> (s -> a) -> Gauge -> IO ()
setGaugeFrom s f = Gauge.set (fromIntegral (f s))
