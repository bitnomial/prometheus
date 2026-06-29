
## 2.4.0

*   Add a `reset` function to the
    `System.Metrics.Prometheus.Metric.Histogram` module, which zeroes the
    bucket counts, sum, and observation count while leaving the bucket bounds
    unchanged. This is useful for resetting metrics between tests.
    [#55](https://github.com/bitnomial/prometheus/issues/55)

## 2.3.1

*   Relax the `http-client-tls` upper bound to allow `0.4.*`.
    [#7966](https://github.com/commercialhaskell/stackage/issues/7966)

## 2.3.0

*   Change the `observeAndSample` function from the
    `System.Metrics.Prometheus.Metric.Histogram` module to return the value of
    the sample that was just added, instead of the previous sample.
    This change matches similar functions for `Counter`s and `Gauge`s.
    [#51](https://github.com/bitnomial/prometheus/pull/51)
