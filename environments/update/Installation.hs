{-# LANGUAGE CPP #-}
module Installation (withInstalledVersion, installation, writeUpdateState, freshVersion, leasePath) where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Environment
import System.FilePath
import UpdateState
#ifdef mingw32_HOST_OS
import UpdateNative
#endif

freshVersion :: IO Version
freshVersion = Version . UUID.toString <$> UUID.nextRandom
leasePath :: FilePath -> FilePath
leasePath root = root </> "installation.lock"
installation :: IO (Maybe (FilePath,Version))
installation = getExecutablePath >>= installationOf
writeUpdateState :: FilePath -> UpdateState -> IO ()
#ifdef mingw32_HOST_OS
writeUpdateState root state = do
 nonce <- freshVersion
 publish (statePath root) (statePath root ++ "." ++ versionName nonce ++ ".tmp") (B.toStrict (encode state))
#else
writeUpdateState _ _ = fail "Windows installation publication is unavailable"
#endif
withInstalledVersion :: IO a -> IO a
#ifdef mingw32_HOST_OS
withInstalledVersion action = installation >>= maybe action (\(root,version) -> withLease (leasePath root) False $ \_ -> do
 state <- readUpdateState root
 allowed <- case state of
  Settled selection -> pure (version == current selection)
  Pending pending -> do
   nonce <- lookupEnv "SAPIENS_UPDATE_ATTEMPT"
   preparing <- lookupEnv "SAPIENS_UPDATE_PREPARING"
   pure (version == target pending && nonce == Just (versionName (attempt pending)) && preparing == Just "1")
 unless allowed (fail "This version is not authorized to open application data. Launch Sapiens from its installed shortcut.")
 action)
#else
withInstalledVersion = id
#endif
