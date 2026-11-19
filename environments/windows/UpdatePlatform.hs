{-# LANGUAGE ForeignFunctionInterface #-}
module UpdatePlatform (module UpdatePlatform, module UpdateNative) where

import UpdateNative hiding (Handle)
import qualified UpdateNative as Native (Handle)

import Control.Exception
import Control.Monad
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Word
import Foreign hiding (void)
import Foreign.C
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import System.Directory hiding (executable)
import System.Environment
import System.Exit
import System.FilePath
import System.Info (arch)
import System.IO
import System.Process
import System.Process.Internals (ProcessHandle__(..),withProcessHandle)
import UpdateState

foreign import ccall safe "MessageBoxW" cMessageBox :: Ptr () -> CWString -> CWString -> Word32 -> IO CInt
foreign import ccall unsafe "GetSystemDirectoryW" cSystemDirectory :: CWString -> Word32 -> IO Word32
showUpdateError :: String -> IO ()
showUpdateError message = withCWString message $ \body -> withCWString "Sapiens update" $ \title -> void (cMessageBox nullPtr body title 0x10)

profileExclusions :: [FilePath]
profileExclusions = ["WebView2/Shell","WebView2/Guests"]

payloadName :: String
payloadName = "Sapiens-Payload-" ++ (if arch=="aarch64" then "win-arm64" else "win-x64") ++ ".exe"
versionExecutable :: FilePath -> Version -> FilePath
versionExecutable root version = versionPath root version </> "Sapiens4.exe"
starterDirectory :: IO FilePath
starterDirectory = getExecutablePath >>= canonicalizePath . takeDirectory

verifyPublisher :: FilePath -> FilePath -> IO ()
verifyPublisher installed payload = do
 system <- allocaArray 260 $ \buffer -> do
  count <- cSystemDirectory buffer 260
  unless (count > 0 && count < 260) (fail "Cannot locate system Windows PowerShell")
  peekCWStringLen (buffer,fromIntegral count)
 inherited <- getEnvironment
 let powershell = system </> "WindowsPowerShell/v1.0/powershell.exe"
     overrides = [("PSModulePath",takeDirectory powershell </> "Modules"),("SAPIENS_SIGNED_APP",installed),("SAPIENS_UPDATE_INSTALLER",payload)]
     environment = overrides ++ filter ((`notElem` map (map toLower . fst) overrides) . map toLower . fst) inherited
     signature = "Microsoft.PowerShell.Security\\Get-AuthenticodeSignature -LiteralPath "
     script = unlines
      [ "$ErrorActionPreference = 'Stop'"
      , "$installed = " ++ signature ++ "$env:SAPIENS_SIGNED_APP"
      , "$payload = " ++ signature ++ "$env:SAPIENS_UPDATE_INSTALLER"
      , "if ($installed.Status -ne 'Valid' -or $payload.Status -ne 'Valid' -or"
      , "    $installed.SignatureType -ne 'Authenticode' -or $payload.SignatureType -ne 'Authenticode' -or"
      , "    [Convert]::ToBase64String($installed.SignerCertificate.RawData) -cne [Convert]::ToBase64String($payload.SignerCertificate.RawData)) { exit 1 }"
      , "'verified'"
      ]
 (code,output,_) <- readCreateProcessWithExitCode ((proc powershell ["-NoProfile","-NonInteractive","-WindowStyle","Hidden","-Command",script])
  {env=Just environment,close_fds=True}) ""
 unless (code == ExitSuccess && words output == ["verified"]) $
  fail "Update installer Authenticode signature or Sapiens publisher verification failed"

stagePayload :: FilePath -> FilePath -> FilePath -> IO ()
stagePayload installed payload destination = do
 verifyPublisher installed payload
 createDirectory destination
 let installer = (proc payload ["/VERYSILENT","/SUPPRESSMSGBOXES","/NORESTART","/DIR="++destination])
       {close_fds=True,use_process_jobs=True}
 code <- bracketOnError (createProcess installer) (\(_,_,_,child) ->
  abortJob child `catch` (throwIO . StagingNotJoined)) (\(_,_,_,child) -> waitForProcess child)
 unless (code == ExitSuccess) (fail "Update payload could not be staged")
 exists <- doesFileExist (destination </> "Sapiens4.exe")
 unless exists (fail "Update payload is incomplete")

launchVersion :: FilePath -> Version -> [String] -> IO ()
launchVersion root version args = do
 inherited <- getEnvironment
 void $ createProcess (proc (versionExecutable root version) args)
  {env=Just (filter (\(key,_) -> key /= "SAPIENS_UPDATE_ATTEMPT") inherited),close_fds=True}

data Candidate = Candidate ProcessHandle Handle Handle
withCandidate :: FilePath -> [String] -> [(String,String)] -> (Candidate -> IO a) -> IO a
withCandidate executable args overrides = bracket acquire close
 where
 acquire = do
  inherited <- getEnvironment
  bracketOnError (createProcess (proc executable args)
   {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,use_process_jobs=True,create_group=True,close_fds=True
   ,env=Just (overrides ++ filter (\(key,_) -> key `notElem` map fst overrides) inherited)}) abort $ \pipes ->
    case pipes of
     (Just input,Just output,Nothing,child) -> do
      mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input,output]
      pure (Candidate child input output)
     _ -> fail "Candidate process pipes are unavailable"
 abort (input,output,errors,child) =
  (abortJob child `catch` (throwIO . CandidateNotJoined))
   `finally` closeStreams (catMaybes [input,output,errors])
 close (Candidate _ input output) = closeStreams [input,output]
 closeStreams = mapM_ (\h -> hClose h `catch` \(_::IOException) -> pure ())
candidateMessage :: Candidate -> IO String
candidateMessage candidate@(Candidate _ _ output) = race exited (hGetLine output) >>= either (const (fail "Candidate exited")) pure
 where
 exited = do
  alive <- candidateAlive candidate
  when alive (threadDelay 100000 >> exited)

candidateAlive :: Candidate -> IO Bool
candidateAlive (Candidate child _ _) = withProcessHandle child $ \state -> case state of
 OpenExtHandle handle _ -> (==ProcessRunning) <$> pollProcess handle
 _ -> pure False

sendCandidate :: Candidate -> String -> IO ()
sendCandidate (Candidate _ input _) message = hPutStrLn input message >> hFlush input

candidateOwnership :: Candidate -> Bool -> IO ()
candidateOwnership (Candidate child _ _) commit =
 if commit then withJob child (\job -> candidateJob job True) else abortJob child

abortJob :: ProcessHandle -> IO ()
abortJob child = withJob child (\job -> candidateJob job False) >> void (waitForProcess child)

withJob :: ProcessHandle -> (Native.Handle -> IO a) -> IO a
withJob child action = withProcessHandle child $ \state -> case state of
 OpenExtHandle _ job -> action job
 ClosedHandle _ -> fail "Windows process job is already closed"
 OpenHandle _ -> fail "Windows process has no owned job"
