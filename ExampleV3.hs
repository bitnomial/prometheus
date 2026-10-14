{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Prometheus.V3 as Prom
import qualified Prometheus.V3.Metric.Counter as Counter


type Status = Int


data Method = GET | POST
    deriving (Eq, Generic, Hashable)
instance Prom.IsLabelValue Method where
    toLabelValue = \case
        GET -> "GET"
        POST -> "POST"


fooCounter :: Prom.Counter
fooCounter = Counter.register "foo_total" "Number of foos"
{-# OPAQUE fooCounter #-}


requestsCounter :: Prom.Labelled (Method, Status) Prom.Counter
requestsCounter =
    Prom.register
        . Prom.withLabels ("method", "status") labels
        $ Counter.new "requests_total" "Number of requests"
  where
    labels =
        [ (method, status)
        | method <- [GET, POST]
        , status <- [200, 404, 500]
        ]
{-# OPAQUE requestsCounter #-}


data Request = Request
    { method :: Method
    }


myHandler :: Request -> IO ()
myHandler req = do
    status <- pure 200

    Counter.inc fooCounter
    Counter.inc (Counter.labels (req.method, status) requestsCounter)

    -- Cache it, for a hot loop
    c <- Counter.labels (req.method, status) requestsCounter
    Counter.inc c


main :: IO ()
main = do
    Prom.serveMetrics 9090 -- Serves metrics on separate thread
    myHandler Request{method = GET}
    myHandler Request{method = GET}

    putStrLn ">>> Server running."
    putStrLn ">>> Run `curl localhost:9090` to see metrics"
    putStrLn ">>> Press Ctrl-C to exit"
    forever $ threadDelay 10000000
