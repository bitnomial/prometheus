{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Prometheus.V3.TH (
    forceRegister,
    ImportedMetrics,
    importMetrics,
) where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import GHC.Exts (unsafeCoerce#)
import GHC.Iface.Env (lookupOrigNameCache)
import GHC.Plugins (
    GenModule (..),
    HscEnv (..),
    PkgQual (..),
    mkModuleName,
    mkVarOcc,
    unitString,
 )
import GHC.Tc.Plugin (FindResult (..))
import GHC.Tc.Utils.Monad (TcM, getTopEnv)
import GHC.Types.Name.Cache (NameCache (..))
import GHC.Unit.Finder (findImportedModule)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (mkNameG_v)


-- | Globally-defined metrics are registered to the global registry when they're
-- first used, due to laziness. Use this function in conjunction with
-- 'importMetrics' to evaluate metrics and force them to register to the global
-- registry immediately.
forceRegister :: ImportedMetrics -> IO ()
forceRegister (ImportedMetrics metrics) =
    foldr
        (\(ImportedMetric x) acc -> x `seq` acc)
        (pure ())
        metrics


newtype ImportedMetrics = ImportedMetrics [ImportedMetric]


data ImportedMetric = forall a. ImportedMetric !a


-- | Import metrics with their fully qualified identifier. Works even if the
-- metric is not exported by the module.
importMetrics :: [String] -> ExpQ
importMetrics names = do
    thNames <- mapM lookupHiddenName names
    appE (conE 'ImportedMetrics) $
        listE (map (appE (conE 'ImportedMetric) . varE) thNames)


-- | Look up a single name (qualified or local) directly from GHC's symbol table
--
-- https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
lookupHiddenName :: String -> Q Name
lookupHiddenName str = case splitName str of
    (Nothing, localName) -> do
        -- If it's local to the current module being compiled, a regular mkName works
        pure $ mkName localName
    (Just modStr, varStr) -> do
        -- 1. Find the module from the string representation
        let modName = mkModuleName modStr
        env <- unsafeRunTcM getTopEnv
        findResult <- runIO $ findImportedModule env modName NoPkgQual
        md <- case findResult of
            Found _ md -> pure md
            _ -> fail $ "Could not find module: " ++ modStr
        -- 2. Bypass export lists and target the internal OccName directly
        let occ = mkVarOcc varStr
        nameCache <- liftIO $ readMVar (nsNames (hsc_NC env))
        case lookupOrigNameCache nameCache md occ of
            Nothing -> fail $ "Could not find identifier: " ++ str
            Just _ -> pure ()
        pure $ mkNameG_v (unitString (moduleUnit md)) modStr varStr
  where
    unsafeRunTcM :: TcM a -> Q a
    unsafeRunTcM m = unsafeCoerce# (\_ -> m)

    splitName :: String -> (Maybe String, String)
    splitName s =
        let (pre, s') = Text.breakOnEnd "." (Text.pack s)
         in case Text.stripSuffix "." pre of
                Nothing -> (Nothing, s)
                Just modName -> (Just (Text.unpack modName), Text.unpack s')
