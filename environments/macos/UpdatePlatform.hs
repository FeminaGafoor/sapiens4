{-# LANGUAGE ForeignFunctionInterface #-}
module UpdatePlatform where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Foreign hiding (void)
import Foreign.C
import System.Directory hiding (executable)
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Signals
import System.Process
import System.Timeout
import UpdateState

newtype Lease = Lease CInt
foreign import ccall safe "d2_validate" cValidate :: CString -> IO CInt
foreign import ccall safe "d2_lease" cLease :: CString -> CInt -> IO CInt
foreign import ccall unsafe "close" cClose :: CInt -> IO CInt
foreign import ccall safe "d2_downgrade" cDowngrade :: CInt -> IO CInt
foreign import ccall safe "d2_publish" cPublish :: CString -> CString -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall safe "d2_flush" cFlush :: CString -> CInt -> IO CInt
foreign import ccall safe "d2_copy" cCopy :: CString -> CString -> CInt -> IO CInt
foreign import ccall safe "d2_metadata" cMetadata :: CString -> Ptr Word8 -> IO CInt
foreign import ccall safe "d2_error" cError :: CString -> IO ()
foreign import ccall safe "d2_activate" cActivate :: CString -> IO CInt
foreign import ccall unsafe "d2_hangup" cHangup :: IO CInt

check :: String -> CInt -> IO ()
check operation code = unless (code == 0) (fail (operation ++ " failed (macOS error " ++ show code ++ ")"))
acquireLease :: FilePath -> Bool -> IO Lease
acquireLease path exclusive = leaseWithMode path (if exclusive then 1 else 0)
leaseWithMode :: FilePath -> CInt -> IO Lease
leaseWithMode path mode = withCString path $ \p -> do
 fd <- cLease p mode
 when (fd < 0) (fail "Could not acquire installation lease")
 pure (Lease fd)
releaseLease :: Lease -> IO ()
releaseLease (Lease fd) = void (cClose fd)
withLease :: FilePath -> Bool -> (Lease -> IO a) -> IO a
withLease path exclusive = bracket (acquireLease path exclusive) releaseLease
withIdleLease :: FilePath -> IO a -> IO a
withIdleLease path = bracket (leaseWithMode path 2) releaseLease . const
downgradeLease :: Lease -> IO ()
downgradeLease (Lease fd) = cDowngrade fd >>= check "Lease handoff"
publish :: FilePath -> FilePath -> B.ByteString -> IO ()
publish path temporary bytes = withCString path $ \p -> withCString temporary $ \t ->
 B.useAsCStringLen bytes $ \(buffer,len) -> cPublish p t (castPtr buffer) (fromIntegral len) >>= check "Durable update decision"
copyOwned, copyMetadata :: FilePath -> FilePath -> IO ()
copyOwned = copyWith 0
copyMetadata = copyWith 1
copyWith :: CInt -> FilePath -> FilePath -> IO ()
copyWith mode source destination = withCString source $ \s -> withCString destination $ \d -> cCopy s d mode >>= check "Owned file copy"
metadata :: FilePath -> IO B.ByteString
metadata path = withCString path $ \p -> allocaBytes 32 $ \buffer -> do
 cMetadata p buffer >>= check ("Owned metadata: " ++ path)
 B.packCStringLen (castPtr buffer,32)

showUpdateError :: String -> IO ()
showUpdateError message = hPutStrLn stderr message >> withCString message cError
validatePath :: FilePath -> IO ()
validatePath path = pathPresent path >>= \exists -> unless exists (fail ("Owned path is missing: " ++ path))
pathPresent :: FilePath -> IO Bool
pathPresent path = do
 let parent = takeDirectory (dropTrailingPathSeparator path)
 present <- if equalFilePath parent path then pure True else pathPresent parent
 if not present then pure False else do
  code <- withCString path cValidate
  if code == 2 then pure False else check ("Owned path validation: " ++ path) code >> pure True
containsPath :: FilePath -> FilePath -> Bool
containsPath parent path = let p = map toLower (normalise parent); q = map toLower (normalise path) in p == q || addTrailingPathSeparator p `isPrefixOf` q
profileExclusions :: [FilePath]
profileExclusions = []
payloadName :: String
payloadName = "Sapiens-Payload-mac-arm64.dmg"
versionExecutable :: FilePath -> Version -> FilePath
versionExecutable root version = versionPath root version </> "Sapiens4.app/Contents/MacOS/Sapiens4"
starterDirectory :: IO FilePath
starterDirectory = do
 override <- lookupEnv "SAPIENS_INSTALL_DIR"
 maybe ((</> "Library/Application Support/Sapiens4/installation") <$> getHomeDirectory) makeAbsolute override

-- Both a signed image and its extracted app must belong to the installed
-- Developer ID publisher. App identity additionally satisfies the old app's DR.
verifyPublisher :: FilePath -> FilePath -> IO ()
verifyPublisher current payload = do
 let app executable = takeDirectory (takeDirectory (takeDirectory executable))
 callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",app current]
 (_,_,description) <- readProcessWithExitCode "/usr/bin/codesign" ["-d","--verbose=4",app current] ""
 team <- case mapMaybe (stripPrefix "TeamIdentifier=") (lines description) of
  [value] | length value == 10 && all (`elem` ['A'..'Z']++['0'..'9']) value -> pure value
  _ -> fail "Automatic updates require an installed Developer ID signed app"
 let target = if takeExtension payload == ".dmg" then payload else app payload
 callProcess "/usr/bin/codesign" ["--verify","--deep","--strict","-R=anchor apple generic and certificate leaf[subject.OU] = \""++team++"\" and certificate leaf[field.1.2.840.113635.100.6.1.13] exists",target]
 when (takeExtension payload /= ".dmg") $ do
  (_,_,requirement) <- readProcessWithExitCode "/usr/bin/codesign" ["-d","-r-",app current] ""
  dr <- case mapMaybe (stripPrefix "designated => ") (lines requirement) of
   [value] -> pure value
   _ -> fail "Installed app has no designated requirement"
  callProcess "/usr/bin/codesign" ["--verify","--strict","-R="++dr,target]
  callProcess "/usr/sbin/spctl" ["--assess","--type","execute",target]

stagePayload :: FilePath -> FilePath -> IO ()
stagePayload payload destination = do
 createDirectory destination
 let mount = destination </> "mount"
 createDirectory mount
 bracket_ (void (readProcess "/usr/bin/hdiutil" ["attach","-readonly","-nobrowse","-mountpoint",mount,payload] ""))
  (void (readProcess "/usr/bin/hdiutil" ["detach",mount] "")) $ do
   let source = mount </> "Sapiens4.app"
   callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",source]
   callProcess "/usr/bin/ditto" [source,destination </> "Sapiens4.app"]
 removeDirectory mount
 let app = destination </> "Sapiens4.app"
 callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",app]
 ident <- readProcess "/usr/libexec/PlistBuddy" ["-c","Print :CFBundleIdentifier",app </> "Contents/Info.plist"] ""
 unless (lines ident == ["com.sapiens4.desktop"]) (fail "Unexpected application identity")
 release <- readProcess "/usr/libexec/PlistBuddy" ["-c","Print :CFBundleShortVersionString",app </> "Contents/Info.plist"] ""
 writeFile (destination </> "sapiens-release.version") release
 -- Submit every staged file before publishing Pending, then flush the device.
 let flush path full = withCString path (\p -> cFlush p full >>= check "Staged bundle durability")
     walk path = do
      validatePath path
      directory <- doesDirectoryExist path
      when directory (listDirectory path >>= mapM_ (walk . (path </>)))
      flush path 0
 walk destination
 flush (takeDirectory destination) 1

launchVersion :: FilePath -> Version -> [String] -> IO ()
launchVersion root version args = do
 let executable = versionExecutable root version
 callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",takeDirectory (takeDirectory (takeDirectory executable))]
 active <- if null args then withCString executable cActivate else pure 0
 inherited <- getEnvironment
 when (active == 0) $ void $ createProcess (proc (versionExecutable root version) args)
  {env=Just (filter ((/= "SAPIENS_UPDATE_ATTEMPT") . fst) inherited),close_fds=True,new_session=True}

data Candidate = Candidate ProcessHandle Handle Handle
withCandidate :: FilePath -> [String] -> [(String,String)] -> (Candidate -> IO a) -> IO a
withCandidate executable args overrides = bracket acquire close
 where
 acquire = do
  inherited <- getEnvironment
  (Just input,Just output,_,child) <- createProcess (proc executable args)
   {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,close_fds=True,new_session=True
   ,env=Just (overrides ++ filter ((`notElem` map fst overrides) . fst) inherited)}
  mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input,output]
  pure (Candidate child input output)
 close (Candidate _ input output) = mapM_ (\h -> hClose h `catch` \(_::IOException) -> pure ()) [input,output]
candidateMessage :: Candidate -> IO String
candidateMessage (Candidate _ _ output) = hGetLine output
candidateAlive :: Candidate -> IO Bool
candidateAlive (Candidate child _ _) = (==Nothing) <$> getProcessExitCode child
sendCandidate :: Candidate -> String -> IO ()
sendCandidate (Candidate _ input _) message = hPutStrLn input message >> hFlush input
candidateOwnership :: Candidate -> Bool -> IO ()
candidateOwnership (Candidate child _ _) commit = unless commit $ do
 terminateProcess child
 done <- timeout 10000000 (waitForProcess child)
 when (done == Nothing) $ getPid child >>= mapM_ (signalProcess sigKILL)
 void (waitForProcess child)

-- An abandoned candidate must not keep a shared lease forever. Its backend
-- owns its own lease and shuts down on stdin EOF when this desktop exits.
watchCoordinator :: IO ()
watchCoordinator = do
 candidate <- lookupEnv "SAPIENS_UPDATE_ATTEMPT"
 forM_ candidate $ \_ -> void $ async $ do
  let wait = cHangup >>= \closed -> if closed == 0 then threadDelay 100000 >> wait else pure ()
  wait
  installed <- getExecutablePath >>= installationOf
  selected <- (case installed of
   Nothing -> pure False
   Just (root,version) -> readUpdateState root >>= \state -> pure $ case state of
    Settled s -> current s == version
    Pending _ -> False) `catch` \(_::IOException) -> pure False
  unless selected (signalProcess sigTERM =<< getProcessID')
 where
 getProcessID' = fromIntegral <$> cGetPid
foreign import ccall unsafe "getpid" cGetPid :: IO CInt
