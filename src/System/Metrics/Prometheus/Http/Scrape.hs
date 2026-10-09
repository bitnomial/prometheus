{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Http.Scrape (
    Path,
    serveMetrics,
    serveMetricsT,
    prometheusApp,
) where

import Control.Monad.IO.Class (
    MonadIO,
    liftIO,
 )
import Data.Text (Text)
import Network.HTTP.Types (
    hContentType,
    methodGet,
    status200,
    status404,
 )
import Network.Wai (
    Application,
    Request,
    Response,
    pathInfo,
    requestMethod,
    responseBuilder,
    responseLBS,
 )
import Network.Wai.Handler.Warp (Port, run)

import System.Metrics.Prometheus.Concurrent.RegistryT (
    RegistryT,
    sample,
 )
import System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import System.Metrics.Prometheus.Registry (RegistrySample)


-- | The HTTP web route on which to serve data
--
-- For example:
--
-- * @http://localhost:9090/metrics@ should use a path of @["metrics"]@.
-- * @http://localhost/@ should use a path of @[]@.
type Path = [Text]


serveMetrics :: MonadIO m => Port -> Path -> IO RegistrySample -> m ()
serveMetrics port path = liftIO . run port . prometheusApp path


serveMetricsT :: MonadIO m => Port -> Path -> RegistryT m ()
serveMetricsT port path = liftIO . serveMetrics port path =<< sample


prometheusApp :: Path -> IO RegistrySample -> Application
prometheusApp path runSample request respond
    | isPrometheusRequest path request = respond =<< prometheusResponse <$> runSample
    | otherwise = respond response404
  where
    prometheusResponse = responseBuilder status200 headers . encodeMetrics
    headers = [(hContentType, "text/plain; version=0.0.4")]


response404 :: Response
response404 = responseLBS status404 header404 body404
  where
    header404 = [(hContentType, "text/plain")]
    body404 = "404"


isPrometheusRequest :: Path -> Request -> Bool
isPrometheusRequest path request = isGet && matchesPath
  where
    matchesPath = pathInfo request == path
    isGet = requestMethod request == methodGet
