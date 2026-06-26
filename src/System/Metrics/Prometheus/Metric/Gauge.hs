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

import Control.Applicative ((<$>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)


newtype Gauge a = Gauge {unGauge :: IORef a}
newtype GaugeSample a = GaugeSample {unGaugeSample :: a} deriving (Show)


new :: (Num a) => IO (Gauge a)
new = Gauge <$> newIORef 0


modifyAndSample :: (a -> a) -> Gauge a -> IO (GaugeSample a)
modifyAndSample f = flip atomicModifyIORef' g . unGauge
  where
    g v = (f v, GaugeSample $ f v)


add :: (Num a) => a -> Gauge a -> IO ()
add x g = modifyAndSample (+ x) g >> pure ()


sub :: (Num a) => a -> Gauge a -> IO ()
sub x g = modifyAndSample (subtract x) g >> pure ()


inc :: (Num a) => Gauge a -> IO ()
inc = add 1


dec :: (Num a) => Gauge a -> IO ()
dec = sub 1


set :: a -> Gauge a -> IO ()
set x g = modifyAndSample (const x) g >> pure ()


sample :: Gauge a -> IO (GaugeSample a)
sample = modifyAndSample id
