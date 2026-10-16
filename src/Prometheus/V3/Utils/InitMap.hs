{-# LANGUAGE LambdaCase #-}

module Prometheus.V3.Utils.InitMap (
    InitMap,
    empty,
    fromList,
    toList,
    getOrCreate,
    getOrCreate',
    GetOrCreateResult (..),
    fromGetOrCreateResult,

    -- * Lifted functions
    delete,
) where

import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Tuple (swap)
import UnliftIO.Exception (SomeException, mask, throwIO, trySyncOrAsync)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, readMVar)


-- | An @InitMap@ is a HashMap, where a key can be in one of three states:
-- not initialized, initializing, or ready. The benefit of this approach is the
-- @InitMap@ could be in an @IORef@ but still get atomic initialization with
-- 'resolve'.
newtype InitMap k v = InitMap {unInitMap :: HashMap k (InitState v)}


data InitState a
    = Init (MVar (Either SomeException a))
    | Ready !a


empty :: InitMap k v
empty = InitMap HashMap.empty


fromList :: (Hashable k) => [(k, v)] -> InitMap k v
fromList = InitMap . HashMap.map Ready . HashMap.fromList


-- | Get a list of ready key-value entries.
toList :: InitMap k v -> [(k, v)]
toList (InitMap m) = [(k, v) | (k, Ready v) <- HashMap.toList m]


getOrCreate :: (Hashable k) => k -> IO v -> IORef (InitMap k v) -> IO v
getOrCreate key createVal initMapRef =
    fromGetOrCreateResult <$> getOrCreate' key createVal initMapRef


data GetOrCreateResult a
    = Existed a
    | Created a


instance Functor GetOrCreateResult where
    fmap f = \case
        Existed a -> Existed (f a)
        Created a -> Created (f a)


fromGetOrCreateResult :: GetOrCreateResult a -> a
fromGetOrCreateResult = \case
    Existed a -> a
    Created a -> a


getOrCreate' :: (Hashable k) => k -> IO v -> IORef (InitMap k v) -> IO (GetOrCreateResult v)
getOrCreate' key createVal initMapRef = mask $ \unmask -> do
    initVar0 <- newEmptyMVar
    initState <-
        atomicModifyIORef' initMapRef $
            first InitMap . insertIfMissing key (Init initVar0) . unInitMap
    case initState of
        -- Child is already initialized
        Just (Ready child) -> pure (Existed child)
        -- Child is being initialized right now in another thread
        Just (Init initVar) -> either throwIO (pure . Existed) =<< readMVar initVar
        -- Child needs initialization
        Nothing -> do
            result <- trySyncOrAsync $ unmask createVal
            -- Update map for future readers
            atomicModifyIORef'_ initMapRef $
                InitMap . insertOrDelete key (Ready <$> result) . unInitMap
            -- Unblock waiting threads
            putMVar initVar0 result
            -- Return result for this thread
            either throwIO (pure . Created) result
  where
    insertIfMissing :: (Hashable k) => k -> v -> HashMap k v -> (HashMap k v, Maybe v)
    insertIfMissing k v =
        let f = \case
                Just v' -> (Just v', Just v')
                Nothing -> (Nothing, Just v)
         in swap . HashMap.alterF f k

    insertOrDelete :: (Hashable k) => k -> Either e v -> HashMap k v -> HashMap k v
    insertOrDelete k = \case
        Left _ -> HashMap.delete k
        Right a -> HashMap.insert k a

    atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
    atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())


delete :: (Hashable k) => k -> InitMap k v -> InitMap k v
delete k = InitMap . HashMap.delete k . unInitMap
