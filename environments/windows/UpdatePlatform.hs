{-# LANGUAGE ForeignFunctionInterface #-}
module UpdatePlatform where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Word
import Foreign hiding (void)
import Foreign.C
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.Directory hiding (executable)
import System.Environment
import System.Exit
import System.FilePath
import System.Info (arch)
import System.IO
import System.Process
import System.Process.Internals (ProcessHandle__(..),withProcessHandle)
import UpdateState

newtype Lease = Lease (Ptr ())
foreign import ccall safe "MessageBoxW" cMessageBox :: Ptr () -> CWString -> CWString -> Word32 -> IO CInt
foreign import ccall safe "WaitForSingleObject" waitProcess :: Ptr () -> Word32 -> IO Word32
foreign import ccall safe "d2_validate" cValidate :: CWString -> IO Word32
foreign import ccall safe "d2_lease" cLease :: CWString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "d2_close" cClose :: Ptr () -> IO ()
foreign import ccall safe "d2_downgrade" cDowngrade :: Ptr () -> IO Word32
foreign import ccall safe "d2_publish" cPublish :: CWString -> CWString -> Ptr Word8 -> Word32 -> IO Word32
foreign import ccall safe "d2_copy" cCopy :: CWString -> CWString -> IO Word32
foreign import ccall safe "d2_metadata" cMetadata :: CWString -> Ptr Word8 -> Word32 -> IO Word32
foreign import ccall safe "d2_copy_metadata" cCopyMetadata :: CWString -> CWString -> IO Word32
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
copyMetadata :: FilePath -> FilePath -> IO ()
copyMetadata source destination = withCWString source $ \s -> withCWString destination $ \d -> cCopyMetadata s d >>= check "Owned metadata copy"
metadata :: FilePath -> IO B.ByteString
metadata path = withCWString path $ \p -> allocaBytes 65536 $ \buffer -> do
 count <- cMetadata p buffer 65536
 when (count == 0 || count > 65536) (fail ("Unsupported or unreadable Windows metadata: " ++ path))
 B.packCStringLen (castPtr buffer,fromIntegral count)
verifyPublisher :: FilePath -> FilePath -> IO ()
verifyPublisher current payload = withCWString current $ \s -> withCWString payload $ \p -> cTrust s p >>= check "Publisher signature verification"

showUpdateError :: String -> IO ()
showUpdateError message = withCWString message $ \body -> withCWString "Sapiens update" $ \title -> void (cMessageBox nullPtr body title 0x10)

-- This module is selected by Cabal. It contains Windows operations, never
-- update/commit/recovery decisions.
validatePath :: FilePath -> IO ()
validatePath path = do
 withCWString path cValidate >>= check "Owned path validation"
 let parent = takeDirectory (dropTrailingPathSeparator path)
 unless (equalFilePath parent path) (validatePath parent)

containsPath :: FilePath -> FilePath -> Bool
containsPath parent path = let p = map toLower (normalise parent); q = map toLower (normalise path) in p == q || addTrailingPathSeparator p `isPrefixOf` q

profileExclusions :: [FilePath]
profileExclusions = ["WebView2/Shell","WebView2/Guests"]

payloadName :: String
payloadName = "Sapiens-Payload-" ++ (if arch=="aarch64" then "win-arm64" else "win-x64") ++ ".exe"
versionExecutable :: FilePath -> Version -> FilePath
versionExecutable root version = versionPath root version </> "Sapiens4.exe"
starterDirectory :: IO FilePath
starterDirectory = getExecutablePath >>= canonicalizePath . takeDirectory

stagePayload :: FilePath -> FilePath -> IO ()
stagePayload payload destination = do
 createDirectory destination
 (_,_,_,child) <- createProcess (proc payload ["/VERYSILENT","/SUPPRESSMSGBOXES","/NORESTART","/DIR="++destination]) {close_fds=True}
 code <- waitForProcess child
 unless (code == ExitSuccess) (fail "Update payload could not be staged")
 exists <- doesFileExist (destination </> "Sapiens4.exe")
 unless exists (fail "Update payload is incomplete")

launchVersion :: FilePath -> Version -> [String] -> IO ()
launchVersion root version args = do
 inherited <- getEnvironment
 void $ createProcess (proc (versionExecutable root version) args)
  {env=Just (filter (\(key,_) -> key /= "SAPIENS_UPDATE_ATTEMPT") inherited),close_fds=True}

data Candidate = Candidate ProcessHandle Handle Handle Handle
withCandidate :: FilePath -> [String] -> [(String,String)] -> (Candidate -> IO a) -> IO a
withCandidate executable args overrides = bracket acquire close
 where
 acquire = do
  inherited <- getEnvironment
  (Just input,Just output,Just errors,child) <- createProcess (proc executable args)
   {std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,use_process_jobs=True,create_group=True,close_fds=True
   ,env=Just (overrides ++ filter (\(key,_) -> key `notElem` map fst overrides) inherited)}
  let candidate = Candidate child input output errors
  (mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input,output,errors]
   >> pure candidate) `onException` (candidateOwnership candidate False >> close candidate)
 close (Candidate _ input output errors) = mapM_ (\h -> hClose h `catch` \(_::IOException) -> pure ()) [input,output,errors]

candidateMessage :: Candidate -> IO String
candidateMessage candidate@(Candidate _ _ output _) = race exited (hGetLine output) >>= either (const (fail "Candidate exited")) pure
 where
 exited = do
  alive <- candidateAlive candidate
  when alive (threadDelay 100000 >> exited)

candidateAlive :: Candidate -> IO Bool
candidateAlive (Candidate child _ _ _) = withProcessHandle child $ \state -> case state of
 OpenExtHandle handle _ -> (/=0) <$> waitProcess handle 0
 _ -> pure False

sendCandidate :: Candidate -> String -> IO ()
sendCandidate (Candidate _ input _ _) message = hPutStrLn input message >> hFlush input

drainCandidate :: Candidate -> IO a -> IO a
drainCandidate (Candidate _ _ _ errors) action = withAsync loop (const action)
 where loop = B.hGetSome errors 4096 >>= \chunk -> unless (B.null chunk) loop

candidateOwnership :: Candidate -> Bool -> IO ()
candidateOwnership (Candidate child _ _ _) commit = withProcessHandle child $ \state -> case state of
 OpenExtHandle _ handle -> cJob handle (if commit then 1 else 0) >>= check "Candidate process ownership"
 _ -> fail "Candidate has no owned Windows job"
