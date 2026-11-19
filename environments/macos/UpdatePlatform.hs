module UpdatePlatform (module UpdatePlatform, module UpdateNative) where

import UpdateNative
import qualified ObjC as O
import qualified Data.Text as T

import Control.Concurrent (threadDelay, threadWaitRead)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Char (isAsciiUpper, isDigit)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
import System.Directory hiding (executable)
import System.Environment
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.IO (stdInput)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Posix.Signals
import System.Process
import System.Timeout
import UpdateState

showUpdateError :: String -> IO ()
showUpdateError message = hPutStrLn stderr message >> O.withAutoreleasePool (do
 app <- O.cls "NSApplication" >>= (`O.obj0` "sharedApplication")
 void (O.obj1 app "setActivationPolicy:" (O.int 1))
 bracket (O.new "NSAlert") (void . (`O.obj0` "release")) $ \alert -> do
  O.setText alert "setMessageText:" "Sapiens4 update could not finish"
  O.setText alert "setInformativeText:" (T.pack message)
  O.str "OK" >>= void . O.obj1 alert "addButtonWithTitle:"
  void (O.obj1 app "activateIgnoringOtherApps:" (O.flag True))
  void (O.long alert "runModal"))

activateVersion :: FilePath -> IO Bool
activateVersion executable = O.withAutoreleasePool $ do
 target <- canonicalizePath executable
 apps <- O.cls "NSWorkspace" >>= (`O.obj0` "sharedWorkspace") >>= (`O.obj0` "runningApplications") >>= O.objects
 let activate [] = pure False
     activate (app:rest) = do
      path <- O.obj0 app "executableURL" >>= (`O.obj0` "path") >>= (`O.obj0` "stringByResolvingSymlinksInPath") >>= O.string
      terminated <- O.booleanValue app "isTerminated"
      if path==T.pack target && not terminated
       then O.obj1 app "activateWithOptions:" (O.int 3) >> pure True
       else activate rest
 activate apps

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

appBundle :: FilePath -> FilePath
appBundle = takeDirectory . takeDirectory . takeDirectory

verifyBundle :: FilePath -> IO ()
verifyBundle path = callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",path]

verifyPublisher :: FilePath -> FilePath -> IO (FilePath -> IO ())
verifyPublisher installedExecutable payload = do
 let installed = appBundle installedExecutable
 (status,_,details) <- readProcessWithExitCode "/usr/bin/codesign" ["--display","--verbose=4",installed] ""
 team <- case [value | Just value <- map (stripPrefix "TeamIdentifier=") (lines details)] of
  [value] | status == ExitSuccess, length value == 10, all (\c -> isAsciiUpper c || isDigit c) value -> pure value
  _ -> fail "The installed app has no Developer ID publisher for updates"
 let publisher = "anchor apple generic and certificate 1[field.1.2.840.113635.100.6.2.6] exists"
              ++ " and certificate leaf[field.1.2.840.113635.100.6.1.13] exists and certificate leaf[subject.OU] = " ++ show team
     verify requirement flags path = callProcess "/usr/bin/codesign" (["--verify","--strict","-R=" ++ requirement] ++ flags ++ [path])
     verifyApp = verify ("identifier \"com.sapiens4.desktop\" and " ++ publisher) ["--deep"]
 verifyApp installed
 verify publisher [] payload
 pure verifyApp

stagePayload :: FilePath -> FilePath -> FilePath -> IO ()
stagePayload installed payload destination = do
 verifyApp <- verifyPublisher installed payload
 stageVerifiedPayload verifyApp payload destination

stageSeed :: FilePath -> FilePath -> IO ()
stageSeed = stageVerifiedPayload verifyBundle

stageVerifiedPayload :: (FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
stageVerifiedPayload verifyApp payload destination = do
 createDirectory destination
 let mount = destination </> "mount"
 createDirectory mount
 bracket_ (void (readProcess "/usr/bin/hdiutil" ["attach","-readonly","-nobrowse","-mountpoint",mount,payload] ""))
  (void (readProcess "/usr/bin/hdiutil" ["detach",mount] "")) $ do
   let source = mount </> "Sapiens4.app"
   verifyApp source
   callProcess "/usr/bin/ditto" [source,destination </> "Sapiens4.app"]
 removeDirectory mount
 let app = destination </> "Sapiens4.app"
 verifyApp app
 ident <- readProcess "/usr/libexec/PlistBuddy" ["-c","Print :CFBundleIdentifier",app </> "Contents/Info.plist"] ""
 unless (lines ident == ["com.sapiens4.desktop"]) (fail "Unexpected application identity")
 release <- readProcess "/usr/libexec/PlistBuddy" ["-c","Print :CFBundleShortVersionString",app </> "Contents/Info.plist"] ""
 writeFile (destination </> "sapiens-release.version") release
 let walk path = do
      validatePath path
      directory <- doesDirectoryExist path
      when directory (listDirectory path >>= mapM_ (walk . (path </>)))
      flush path False
 walk destination
 flush (takeDirectory destination) True

launchVersion :: FilePath -> Version -> [String] -> IO ()
launchVersion root version args = do
 let executable = versionExecutable root version
 verifyBundle (appBundle executable)
 active <- if null args then activateVersion executable else pure False
 inherited <- getEnvironment
 unless active $ void $ createProcess (proc executable args)
  {env=Just (filter ((/= "SAPIENS_UPDATE_ATTEMPT") . fst) inherited),close_fds=True,new_session=True}

data Candidate = Candidate ProcessHandle ProcessID Handle Handle
withCandidate :: FilePath -> [String] -> [(String,String)] -> (Candidate -> IO a) -> IO a
withCandidate executable args overrides = bracket acquire close
 where
 acquire = do
  inherited <- getEnvironment
  bracketOnError (createProcess (proc executable args)
   {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,close_fds=True,new_session=True
   ,env=Just (overrides ++ filter ((`notElem` map fst overrides) . fst) inherited)}) abort $
   \pipes -> case pipes of
    (Just input,Just output,Nothing,child) -> do
     mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input,output]
     pid <- getPid child >>= maybe (fail "Candidate process ID unavailable") pure
     pure (Candidate child pid input output)
    _ -> fail "Candidate process pipes are unavailable"
 abort (input,output,errors,child) =
  (do
   getPid child >>= mapM_ (\pid -> catchJust (guard . isDoesNotExistError) (signalProcessGroup sigKILL pid) (const (pure ())))
   void (waitForProcess child))
  `finally` closeStreams (catMaybes [input,output,errors])
 close (Candidate _ _ input output) = closeStreams [input,output]
 closeStreams = mapM_ (\h -> hClose h `catch` \(_::IOException) -> pure ())
candidateMessage :: Candidate -> IO String
candidateMessage (Candidate _ _ _ output) = hGetLine output
candidateAlive :: Candidate -> IO Bool
candidateAlive (Candidate child _ _ _) = (==Nothing) <$> getProcessExitCode child
sendCandidate :: Candidate -> String -> IO ()
sendCandidate (Candidate _ _ input _) message = hPutStrLn input message >> hFlush input
candidateOwnership :: Candidate -> Bool -> IO ()
candidateOwnership (Candidate child pid _ _) commit = unless commit $ do
 let absent action = catchJust (guard . isDoesNotExistError) action (const (pure ()))
     join = do
      void (waitForProcess child)
      let wait = do
           alive <- catchJust (guard . isDoesNotExistError) (signalProcessGroup nullSignal pid >> pure True) (const (pure False))
           when alive (threadDelay 100000 >> wait)
      wait
 absent (signalProcessGroup sigTERM pid)
 done <- timeout 10000000 join
 when (done == Nothing) $ do
  absent (signalProcessGroup sigKILL pid)
  joined <- timeout 10000000 join
  when (joined == Nothing) (fail "Candidate process group did not exit")

watchCoordinator :: IO ()
watchCoordinator = do
 candidate <- lookupEnv "SAPIENS_UPDATE_ATTEMPT"
 forM_ candidate $ \_ -> void $ async $ do
  -- A queued commit or EOF wakes the watcher without consuming the message.
  threadWaitRead stdInput
  installed <- getExecutablePath >>= installationOf
  selected <- (case installed of
   Nothing -> pure False
   Just (root,version) -> readUpdateState root >>= \state -> pure $ case state of
    Settled s -> current s == version
    Pending _ -> False) `catch` \(_::IOException) -> pure False
  unless selected (signalProcess sigTERM =<< getProcessID)
