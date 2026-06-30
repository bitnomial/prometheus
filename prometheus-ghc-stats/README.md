# prometheus-ghc-stats

Export GHC RTS metrics (from [`GHC.Stats`](https://hackage.haskell.org/package/base/docs/GHC-Stats.html))
through the [`prometheus`](http://github.com/bitnomial/prometheus) registry.

`registerGHCStats` registers a metric handle for each `RTSStats` field in the
current `RegistryT` and returns an `IO ()` action that snapshots the RTS stats
and writes each field to its handle. Compose that updater with the registry
sample so every scrape sees fresh values:

```haskell
import System.Metrics.Prometheus.Concurrent.RegistryT (runRegistryT, sample)
import System.Metrics.Prometheus.GHCStats (registerGHCStats)
import System.Metrics.Prometheus.Http.Scrape (serveMetrics)

main :: IO ()
main = do
    (updater, regSample) <- runRegistryT $ (,) <$> registerGHCStats <*> sample
    serveMetrics 8080 ["metrics"] (updater >> regSample)
```

## Requirements

* The executable must be run with `+RTS -T` to enable the RTS statistics.
  Otherwise `getRTSStatsEnabled` returns `False`, the updater is a no-op, and
  every metric reports `0`.
* Requires GHC >= 9.10.

## Notes

Time fields are exposed in nanoseconds (the unit `GHC.Stats` uses); divide by
`1e9` in PromQL for seconds. The `Counter` type in this `prometheus` fork is
`Int`-backed, so fractional seconds cannot be represented losslessly through
it.
