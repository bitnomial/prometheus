module System.Metrics.Prometheus.Metric.Gauge (
    Gauge,
    GaugeSample (..),
    new,
    add,
    sub,
    inc,
    dec,
    set,
    sample,
    modifyAndSample,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)


newtype Gauge = Gauge {unGauge :: IORef Double}
newtype GaugeSample = GaugeSample {unGaugeSample :: Double} deriving Show


new :: IO Gauge
new = Gauge <$> newIORef 0


modifyAndSample :: (Double -> Double) -> Gauge -> IO GaugeSample
modifyAndSample f = flip atomicModifyIORef' g . unGauge
  where
    g v = (f v, GaugeSample $ f v)


add :: Double -> Gauge -> IO ()
add x g = modifyAndSample (+ x) g >> pure ()


sub :: Double -> Gauge -> IO ()
sub x g = modifyAndSample (subtract x) g >> pure ()


inc :: Gauge -> IO ()
inc = add 1


dec :: Gauge -> IO ()
dec = sub 1


set :: Double -> Gauge -> IO ()
set x g = modifyAndSample (const x) g >> pure ()


sample :: Gauge -> IO GaugeSample
sample = modifyAndSample id
