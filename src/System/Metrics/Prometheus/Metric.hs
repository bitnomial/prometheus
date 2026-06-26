module System.Metrics.Prometheus.Metric where

import System.Metrics.Prometheus.Metric.Counter (
    Counter,
    CounterSample,
 )
import System.Metrics.Prometheus.Metric.Gauge (Gauge, GaugeSample)
import System.Metrics.Prometheus.Metric.Histogram (
    Histogram,
    HistogramSample,
 )
import System.Metrics.Prometheus.Metric.Summary (SummarySample)


data Metric
    = CounterMetric Counter
    | GaugeMetric (Gauge Double) -- \| Summary S.Summary
    | HistogramMetric Histogram


data MetricSample
    = CounterMetricSample CounterSample
    | GaugeMetricSample (GaugeSample Double)
    | HistogramMetricSample HistogramSample
    | SummaryMetricSample SummarySample


metricSample ::
    (CounterSample -> a) ->
    (GaugeSample Double -> a) ->
    (HistogramSample -> a) ->
    (SummarySample -> a) ->
    MetricSample ->
    a
metricSample f _ _ _ (CounterMetricSample s) = f s
metricSample _ f _ _ (GaugeMetricSample s) = f s
metricSample _ _ f _ (HistogramMetricSample s) = f s
metricSample _ _ _ f (SummaryMetricSample s) = f s
