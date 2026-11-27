{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Http.Push (
    pushMetrics,
    parseURI,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Base64.URL as Base64URL
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map (foldMapWithKey)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (
    Request (..),
    RequestBody (..),
    getUri,
    httpNoBody,
    parseRequest,
    requestBody,
    requestFromURI,
    requestHeaders,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, methodPut)
import Network.URI (
    URI (..),
    URIAuth,
    escapeURIString,
    isUnreserved,
    nullURI,
 )

import System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import System.Metrics.Prometheus.MetricId (Labels (..))
import System.Metrics.Prometheus.Registry (RegistrySample)


-- | Parses a uri such that
-- @
--   parseURI "https://example.com"
--      ===
--   Just (URI "https:" "//example.com"
-- @
parseURI :: String -> Maybe URI
parseURI = fmap getUri . parseRequest


pushMetrics ::
    -- | PushGateway URI name, including port number (ex: @parseUri https://myGateway.com:8080@)
    URI ->
    -- | Job name
    Text ->
    -- | Label set to use as a grouping key for metrics
    Labels ->
    -- | Microsecond push frequency
    Int ->
    -- | Action to get latest metrics
    IO RegistrySample ->
    IO ()
pushMetrics gatewayURI jobName labels frequencyMicros getSample = do
    manager <- newTlsManager
    gn <- maybe (error "Invalid URI Authority") pure gatewayName
    requestUri <- requestFromURI $ buildUri scheme gn jobName labels
    forever $ getSample >>= flip httpNoBody manager . request requestUri >> threadDelay frequencyMicros
  where
    URI scheme gatewayName _ _ _ = gatewayURI
    request req sample =
        req
            { method = methodPut
            , requestBody = RequestBodyLBS . toLazyByteString $ encodeMetrics sample
            , requestHeaders = [(hContentType, "text/plain; version=0.0.4")]
            }


buildUri :: String -> URIAuth -> Text -> Labels -> URI
buildUri scheme gatewayName jobName (Labels ls) =
    nullURI
        { uriScheme = scheme
        , uriAuthority = Just gatewayName
        , uriPath = "/metrics" ++ groupingKeyPath "job" jobName ++ foldMapWithKey groupingKeyPath ls
        }


-- | One @/name/value@ pair of the grouping key in the Pushgateway URL. As in
-- the Go client, a value that is empty or contains a @/@ is sent base64url
-- encoded (@/name\@base64/value@), since the Pushgateway would otherwise split
-- it into more path segments; other values are percent-encoded.
groupingKeyPath :: Text -> Text -> String
groupingKeyPath k v
    | Text.null v = "/" ++ unpack k ++ "@base64/="
    | Text.any (== '/') v = "/" ++ unpack k ++ "@base64/" ++ BS8.unpack (Base64URL.encodeUnpadded (encodeUtf8 v))
    | otherwise = "/" ++ unpack k ++ "/" ++ escapeURIString isUnreserved (unpack v)
