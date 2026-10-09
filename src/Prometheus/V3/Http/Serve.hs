{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Http.Serve (
    serveMetrics,
    serveMetricsWith,
    Options (..),
    defaultOptions,
    app,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (
    Application,
    Request,
    Response,
 )
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Prometheus.V3.Encode (EscapeScheme (EscapingUnderscores))
import qualified Prometheus.V3.Encode as Encode
import Prometheus.V3.Registry (Registry, globalRegistry)
import qualified Prometheus.V3.Registry as Registry
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO)


data Options = Options
    { port :: Port
    , path :: [Text]
    -- ^ The path to serve the endpoint.
    --
    -- For example:
    --   * @/@ => @[]@
    --   * @/metrics@ => @["metrics"]@
    , registry :: Registry
    }


defaultOptions :: Options
defaultOptions =
    Options
        { port = 9090
        , path = []
        , registry = globalRegistry
        }


serveMetrics :: (MonadUnliftIO m) => Port -> m ()
serveMetrics port = serveMetricsWith defaultOptions{port = port}


serveMetricsWith :: (MonadUnliftIO m) => Options -> m ()
serveMetricsWith opts = void . forkIO . liftIO $ Warp.run opts.port (app opts)


app :: Options -> Application
app opts request respond = (respond =<<) $ do
    case (HTTP.parseMethod request.requestMethod, request.pathInfo) of
        (Right HTTP.GET, path) | path == opts.path -> metricsResponse opts request
        _ -> pure response404


metricsResponse :: Options -> Request -> IO Response
metricsResponse opts _ = do
    samples <- Registry.sample opts.registry
    let body = Encode.encodeRegistrySample encodeOpts samples
    pure $ Wai.responseBuilder HTTP.status200 headers body
  where
    headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
    encodeOpts =
        Encode.Options
            { escapeScheme = EscapingUnderscores -- TODO: parse from Accept header
            }


response404 :: Response
response404 = Wai.responseLBS HTTP.status404 headers body
  where
    headers = [(HTTP.hContentType, "text/plain")]
    body = ""
