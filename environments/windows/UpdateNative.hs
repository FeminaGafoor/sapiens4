{-# LANGUAGE ForeignFunctionInterface #-}
module UpdateNative where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Word
import Foreign hiding (void)
import Foreign.C

newtype Lease = Lease (Ptr ())
foreign import ccall safe "MessageBoxW" cMessageBox :: Ptr () -> CWString -> CWString -> Word32 -> IO CInt
foreign import ccall safe "d2_lease" cLease :: CWString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "d2_close" cClose :: Ptr () -> IO ()
foreign import ccall safe "d2_downgrade" cDowngrade :: Ptr () -> IO Word32
foreign import ccall safe "d2_publish" cPublish :: CWString -> CWString -> Ptr Word8 -> Word32 -> IO Word32
foreign import ccall safe "d2_copy" cCopy :: CWString -> CWString -> IO Word32
foreign import ccall safe "d2_metadata" cMetadata :: CWString -> Ptr Word8 -> Word32 -> IO Word32
foreign import ccall safe "d2_directory_metadata" cDirectoryMetadata :: CWString -> CWString -> IO Word32
foreign import ccall safe "d2_trust" cTrust :: CWString -> CWString -> IO Word32
foreign import ccall safe "d2_job" cJob :: Ptr () -> CInt -> IO Word32

check :: String -> Word32 -> IO ()
check operation code = unless (code == 0) (fail (operation ++ " failed (Windows error " ++ show code ++ ")"))
acquireLease :: FilePath -> Bool -> IO Lease
acquireLease path exclusive = withCWString path $ \p -> do
 handle <- cLease p (if exclusive then 1 else 0)
 when (handle == nullPtr) (fail "Could not acquire installation lease")
 pure (Lease handle)
releaseLease :: Lease -> IO ()
releaseLease (Lease handle) = cClose handle
withLease :: FilePath -> Bool -> (Lease -> IO a) -> IO a
withLease path exclusive = bracket (acquireLease path exclusive) releaseLease
downgradeLease :: Lease -> IO ()
downgradeLease (Lease handle) = cDowngrade handle >>= check "Lease handoff"
publish :: FilePath -> FilePath -> B.ByteString -> IO ()
publish path temporary bytes = withCWString path $ \p -> withCWString temporary $ \t ->
 B.useAsCStringLen bytes $ \(buffer,len) -> cPublish p t (castPtr buffer) (fromIntegral len) >>= check "Durable update decision"
copyOwned :: FilePath -> FilePath -> IO ()
copyOwned source destination = withCWString source $ \s -> withCWString destination $ \d -> cCopy s d >>= check "Owned file copy"
directoryMetadata :: FilePath -> FilePath -> IO ()
directoryMetadata source destination = withCWString source $ \s -> withCWString destination $ \d -> cDirectoryMetadata s d >>= check "Directory metadata"
metadata :: FilePath -> IO B.ByteString
metadata path = withCWString path $ \p -> allocaBytes 65536 $ \buffer -> do
 count <- cMetadata p buffer 65536
 when (count == 0 || count > 65536) (fail ("Unsupported or unreadable Windows metadata: " ++ path))
 B.packCStringLen (castPtr buffer,fromIntegral count)
verifyPublisher :: FilePath -> FilePath -> IO ()
verifyPublisher current payload = withCWString current $ \s -> withCWString payload $ \p -> cTrust s p >>= check "Publisher signature verification"

showUpdateError :: String -> IO ()
showUpdateError message = withCWString message $ \body -> withCWString "Sapiens update" $ \title -> void (cMessageBox nullPtr body title 0x10)
