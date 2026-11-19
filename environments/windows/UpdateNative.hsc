{-# LANGUAGE ForeignFunctionInterface #-}
module UpdateNative where

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0602
#endif
#include <windows.h>
#include <aclapi.h>
#include <sddl.h>

import Control.Concurrent (threadDelay, runInBoundThread)
import Control.Exception
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString as B
import Foreign hiding (void)
import Foreign.C
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath (takeFileName)
import System.IO.Error
import UpdateState (pathPresentWith)

type Handle = Ptr ()
foreign import ccall safe "CreateFileW" create :: CWString -> Word32 -> Word32 -> Ptr () -> Word32 -> Word32 -> Handle -> IO Handle
foreign import ccall unsafe "CloseHandle" close :: Handle -> IO CInt
foreign import ccall unsafe "WaitForSingleObject" waitProcess :: Handle -> Word32 -> IO Word32
foreign import ccall unsafe "GetLastError" lastError :: IO Word32
foreign import ccall safe "GetFinalPathNameByHandleW" getFinalPath :: Handle -> CWString -> Word32 -> Word32 -> IO Word32
foreign import ccall safe "GetFileInformationByHandle" information :: Handle -> Ptr () -> IO CInt
foreign import ccall safe "LockFileEx" lock :: Handle -> Word32 -> Word32 -> Word32 -> Word32 -> Ptr () -> IO CInt
foreign import ccall safe "UnlockFileEx" unlock :: Handle -> Word32 -> Word32 -> Word32 -> Ptr () -> IO CInt
foreign import ccall safe "WriteFile" write :: Handle -> Ptr a -> Word32 -> Ptr Word32 -> Ptr () -> IO CInt
foreign import ccall safe "ReadFile" readBytes :: Handle -> Ptr a -> Word32 -> Ptr Word32 -> Ptr () -> IO CInt
foreign import ccall safe "FlushFileBuffers" flush :: Handle -> IO CInt
foreign import ccall safe "MoveFileExW" move :: CWString -> CWString -> Word32 -> IO CInt
foreign import ccall safe "DeleteFileW" delete :: CWString -> IO CInt
foreign import ccall safe "CopyFileW" copy :: CWString -> CWString -> CInt -> IO CInt
foreign import ccall safe "GetFileAttributesW" attributes :: CWString -> IO Word32
foreign import ccall safe "SetFileAttributesW" setAttributes :: CWString -> Word32 -> IO CInt
foreign import ccall safe "SetFileTime" setTimes :: Handle -> Ptr () -> Ptr () -> Ptr () -> IO CInt
foreign import ccall safe "GetNamedSecurityInfoW" getSecurity :: CWString -> CInt -> Word32 -> Ptr Handle -> Ptr Handle -> Ptr Handle -> Ptr Handle -> Ptr Handle -> IO Word32
foreign import ccall safe "SetNamedSecurityInfoW" setSecurity :: CWString -> CInt -> Word32 -> Handle -> Handle -> Handle -> Handle -> IO Word32
foreign import ccall safe "SetFileSecurityW" setFileSecurity :: CWString -> Word32 -> Handle -> IO CInt
foreign import ccall unsafe "GetSecurityDescriptorControl" securityControl :: Handle -> Ptr Word16 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "EqualSid" equalSid :: Handle -> Handle -> IO CInt
foreign import ccall safe "ConvertSecurityDescriptorToStringSecurityDescriptorW" securityText :: Handle -> Word32 -> Word32 -> Ptr CWString -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "LocalFree" freeLocal :: Ptr a -> IO (Ptr a)
foreign import ccall safe "FindFirstStreamW" firstStream :: CWString -> CInt -> Ptr () -> Word32 -> IO Handle
foreign import ccall safe "FindNextStreamW" nextStream :: Handle -> Ptr () -> IO CInt
foreign import ccall unsafe "FindClose" closeFind :: Handle -> IO CInt
foreign import ccall safe "QueryInformationJobObject" queryJob :: Handle -> CInt -> Ptr () -> Word32 -> Ptr Word32 -> IO CInt
foreign import ccall safe "SetInformationJobObject" setJob :: Handle -> CInt -> Ptr () -> Word32 -> IO CInt
foreign import ccall safe "TerminateJobObject" terminateJob :: Handle -> Word32 -> IO CInt

status :: String -> Word32 -> IO ()
status operation code = unless (code==0) $ ioError $ mkIOError
 (if code `elem` [2,3] then doesNotExistErrorType else userErrorType) (operation ++ ": Windows error " ++ show code) Nothing Nothing
check :: String -> IO CInt -> IO ()
check operation action = runInBoundThread $ action >>= \ok -> when (ok==0) (lastError >>= status operation)
data ProcessObservation = ProcessRunning | ProcessExited deriving Eq

pollProcess :: Handle -> IO ProcessObservation
pollProcess process = runInBoundThread $ do
 result <- waitProcess process 0
 case result of
  #{const WAIT_OBJECT_0} -> pure ProcessExited
  #{const WAIT_TIMEOUT} -> pure ProcessRunning
  #{const WAIT_FAILED} -> do
   code <- lastError
   fail ("Cannot observe Windows process: Windows error " ++ show code)
  #{const WAIT_ABANDONED} -> fail "Windows reported an abandoned mutex while observing a process"
  _ -> fail ("Unexpected Windows process wait result: " ++ show result)

zeroed :: Int -> (Ptr () -> IO a) -> IO a
zeroed size action = allocaBytes size $ \p -> fillBytes p 0 size >> action p
open :: FilePath -> Word32 -> Word32 -> Word32 -> Word32 -> IO Handle
open path access share creation flags = runInBoundThread $ withCWString path $ \p -> do
 h <- create p access share nullPtr creation flags nullPtr
 when (h==intPtrToPtr (-1)) (lastError >>= status path)
 pure h
withFile :: FilePath -> Word32 -> Word32 -> Word32 -> Word32 -> (Handle -> IO a) -> IO a
withFile path access share creation flags = bracket (open path access share creation flags) (void . close)

inspect :: FilePath -> (Ptr () -> IO a) -> IO a
inspect path action = withFile path #{const FILE_READ_ATTRIBUTES} #{const FILE_SHARE_READ} #{const OPEN_EXISTING}
 #{const FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS} $ \h -> allocaBytes #{size BY_HANDLE_FILE_INFORMATION} $ \info -> do
 check path (information h info)
 attrs <- #{peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes} info :: IO Word32
 links <- #{peek BY_HANDLE_FILE_INFORMATION, nNumberOfLinks} info :: IO Word32
 unless (attrs .&. #{const FILE_ATTRIBUTE_REPARSE_POINT | FILE_ATTRIBUTE_ENCRYPTED | FILE_ATTRIBUTE_SPARSE_FILE | FILE_ATTRIBUTE_COMPRESSED | FILE_ATTRIBUTE_OFFLINE} == 0
  && (attrs .&. #{const FILE_ATTRIBUTE_DIRECTORY} /= 0 || links==1)) (fail ("Unsupported owned path: " ++ path))
 action info
validatePath :: FilePath -> IO ()
validatePath path = pathPresent path >>= \exists -> unless exists (fail ("Missing owned path: " ++ path))
pathPresent :: FilePath -> IO Bool
pathPresent = pathPresentWith $ \path -> inspect path (const (pure True)) `catch` \e ->
 if isDoesNotExistError e then pure False else throwIO e
actualName :: FilePath -> IO (Maybe FilePath)
actualName path = catchJust (guard . isDoesNotExistError) readName (const (pure Nothing))
 where
 readName = withFile path 0 #{const FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE} #{const OPEN_EXISTING}
  #{const FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS} $ \h ->
   runInBoundThread $ allocaArray 32768 $ \buffer -> do
    count <- getFinalPath h buffer 32768 #{const VOLUME_NAME_NONE}
    when (count == 0) (lastError >>= status path)
    unless (count < 32768) (fail ("Filesystem name too long: " ++ path))
    Just . takeFileName <$> peekCWStringLen (buffer,fromIntegral count)

withRootIdentity :: FilePath -> (String -> IO a) -> IO a
withRootIdentity path action = validatePath path >> withFile path #{const FILE_READ_ATTRIBUTES} #{const FILE_SHARE_READ | FILE_SHARE_WRITE}
 #{const OPEN_EXISTING} #{const FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS} (\h -> allocaBytes #{size BY_HANDLE_FILE_INFORMATION} $ \p -> do
 check path (information h p)
 attrs <- #{peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes} p :: IO Word32
 unless (attrs .&. #{const FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT} == #{const FILE_ATTRIBUTE_DIRECTORY}) (fail "Data root changed")
 volume <- #{peek BY_HANDLE_FILE_INFORMATION, dwVolumeSerialNumber} p :: IO Word32
 high <- #{peek BY_HANDLE_FILE_INFORMATION, nFileIndexHigh} p :: IO Word32
 low <- #{peek BY_HANDLE_FILE_INFORMATION, nFileIndexLow} p :: IO Word32
 action (show [fromIntegral volume,(fromIntegral high `shiftL` 32) .|. fromIntegral low :: Word64]))

newtype Lease = Lease Handle
acquireLease :: FilePath -> Bool -> IO Lease
acquireLease path exclusive = leaseWithMode path (if exclusive then 1 else 0)
leaseWithMode :: FilePath -> Int -> IO Lease
leaseWithMode path mode = mask $ \restore -> do
 h <- open path #{const GENERIC_READ | GENERIC_WRITE} #{const FILE_SHARE_READ | FILE_SHARE_WRITE}
  #{const OPEN_ALWAYS} #{const FILE_FLAG_OPEN_REPARSE_POINT}
 let validate = allocaBytes #{size BY_HANDLE_FILE_INFORMATION} $ \info -> do
      check path (information h info)
      attrs <- #{peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes} info :: IO Word32
      links <- #{peek BY_HANDLE_FILE_INFORMATION, nNumberOfLinks} info :: IO Word32
      unless (attrs .&. #{const FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT} == 0 && links == 1) $
       fail ("Unsupported lock file: " ++ path)
     acquire = runInBoundThread $ zeroed #{size OVERLAPPED} $ \range -> do
      deadline <- (+15000000000) <$> getMonotonicTimeNSec
      let retry = do
           ok <- lock h (#{const LOCKFILE_FAIL_IMMEDIATELY} .|. if mode==0 then 0 else #{const LOCKFILE_EXCLUSIVE_LOCK}) 0 1 0 range
           when (ok==0) $ do
            err <- lastError
            now <- getMonotonicTimeNSec
            if err== #{const ERROR_LOCK_VIOLATION} && mode/=2 && now<deadline then threadDelay 50000 >> retry else status path err
      retry
 (validate >> restore acquire) `onException` void (close h)
 pure (Lease h)
releaseLease :: Lease -> IO ()
releaseLease (Lease h) = void (close h)
withLease :: FilePath -> Bool -> (Lease -> IO a) -> IO a
withLease path mode = bracket (acquireLease path mode) releaseLease
withIdleLease :: FilePath -> IO a -> IO a
withIdleLease path = bracket (leaseWithMode path 2) releaseLease . const
downgradeLease :: Lease -> IO ()
downgradeLease (Lease h) = zeroed #{size OVERLAPPED} $ \range -> do
 check "Shared lease" (lock h #{const LOCKFILE_FAIL_IMMEDIATELY} 0 1 0 range)
 check "Exclusive lease release" (unlock h 0 1 0 range)

writeBytes :: Handle -> B.ByteString -> IO ()
writeBytes h bytes = B.useAsCStringLen bytes $ \(p,n) -> alloca $ \count -> do
 when (toInteger n>toInteger (maxBound :: Word32)) (fail "Update record is too large")
 check "Write update" (write h p (fromIntegral n) count nullPtr)
 written <- peek count
 unless (fromIntegral written==n) (fail "Incomplete update write")
publish, publishNew :: FilePath -> FilePath -> B.ByteString -> IO ()
publish = publishWith #{const MOVEFILE_REPLACE_EXISTING}
publishNew = publishWith 0
publishWith :: Word32 -> FilePath -> FilePath -> B.ByteString -> IO ()
publishWith flags path temporary bytes = mask $ \restore -> do
 h <- open temporary #{const GENERIC_WRITE} 0 #{const CREATE_NEW} #{const FILE_ATTRIBUTE_NORMAL}
 (bracket (pure h) (check temporary . close) (\file -> restore (writeBytes file bytes >> check temporary (flush file))) >>
  withCWString temporary (\t -> withCWString path (\p -> check path (move t p (flags .|. #{const MOVEFILE_WRITE_THROUGH})))))
  `onException` withCWString temporary (void . delete)

securityFlags :: Word32
securityFlags = #{const OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION}
withSecurity :: FilePath -> (Handle -> Handle -> Handle -> Handle -> IO a) -> IO a
withSecurity path action = allocaArray 4 $ \p -> bracket
 (withCWString path $ \name -> do
  getSecurity name #{const SE_FILE_OBJECT} securityFlags p (p `advancePtr` 1) (p `advancePtr` 2) nullPtr (p `advancePtr` 3) >>= status path
  peekElemOff p 3)
 (void . freeLocal) (\sd -> do owner <- peekElemOff p 0; group <- peekElemOff p 1; acl <- peekElemOff p 2; action owner group acl sd)
copySecurity :: FilePath -> FilePath -> IO ()
copySecurity source destination = withSecurity source $ \owner group acl sd -> withSecurity destination $ \otherOwner otherGroup _ _ ->
 alloca $ \control -> alloca $ \revision -> do
 check source (securityControl sd control revision)
 flags <- peek control
 sameOwner <- equalSid owner otherOwner
 sameGroup <- equalSid group otherGroup
 let selected = #{const DACL_SECURITY_INFORMATION} .|. (if flags .&. #{const SE_DACL_PROTECTED}/=0 then #{const PROTECTED_DACL_SECURITY_INFORMATION} else #{const UNPROTECTED_DACL_SECURITY_INFORMATION})
      .|. (if sameOwner==0 then #{const OWNER_SECURITY_INFORMATION} else 0) .|. (if sameGroup==0 then #{const GROUP_SECURITY_INFORMATION} else 0)
 withCWString destination $ \p -> if flags .&. #{const SE_DACL_AUTO_INHERITED}/=0
  then setSecurity p #{const SE_FILE_OBJECT} selected owner group acl nullPtr >>= status destination
  else check destination (setFileSecurity p selected sd)

streams :: FilePath -> (String -> IO a) -> IO [a]
streams path action = runInBoundThread $ allocaBytes #{size WIN32_FIND_STREAM_DATA} $ \dataPtr ->
 bracket (withCWString path $ \p -> firstStream p #{const FindStreamInfoStandard} dataPtr 0) (\h -> when (h/=intPtrToPtr (-1)) (void (closeFind h))) $ \h -> do
  let end = lastError >>= \err -> if err== #{const ERROR_HANDLE_EOF} then pure [] else status path err >> pure []
      loop = do
       name <- peekCWString (#{ptr WIN32_FIND_STREAM_DATA, cStreamName} dataPtr)
       value <- action name
       more <- nextStream h dataPtr
       rest <- if more==0 then end else loop
       pure (value:rest)
  if h==intPtrToPtr (-1) then end else loop

readChunks :: Handle -> (a -> B.ByteString -> IO a) -> a -> IO a
readChunks h action initial = allocaBytes 65536 $ \buffer -> alloca $ \count -> do
 let loop value = do
      check "Read owned stream" (readBytes h buffer 65536 count nullPtr)
      n <- peek count
      if n==0 then pure value else B.packCStringLen (buffer,fromIntegral n) >>= action value >>= loop
 loop initial
withRead :: FilePath -> (Handle -> IO a) -> IO a
withRead path = withFile path #{const GENERIC_READ} #{const FILE_SHARE_READ} #{const OPEN_EXISTING} 0
copyOwned :: FilePath -> FilePath -> IO ()
copyOwned source destination = inspect source $ \info -> do
 attrs <- #{peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes} info :: IO Word32
 when (attrs .&. #{const FILE_ATTRIBUTE_DIRECTORY}/=0) (fail "Expected a file")
 withCWString source $ \s -> withCWString destination $ \d -> check destination (copy s d 1)
copyMetadata :: Bool -> FilePath -> FilePath -> IO ()
copyMetadata directory source destination = runInBoundThread $ inspect source $ \info -> do
 kind <- #{peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes} info :: IO Word32
 unless (directory == (kind .&. #{const FILE_ATTRIBUTE_DIRECTORY} /= 0)) (fail "Owned path type changed")
 when directory $ do
  validatePath destination
  void $ streams destination $ \name -> withCWString (destination++name) (check destination . delete)
  void $ streams source $ \name -> withRead (source++name) $ \input ->
   withFile (destination++name) #{const GENERIC_WRITE} 0 #{const CREATE_NEW} 0 $ \output -> do
    readChunks input (\_ bytes -> writeBytes output bytes) ()
    check destination (flush output)
 withCWString destination $ \p -> do
  attrs <- attributes p
  when (attrs== #{const INVALID_FILE_ATTRIBUTES}) (lastError >>= status destination)
  check destination (setAttributes p (attrs .&. complement #{const FILE_ATTRIBUTE_READONLY}))
  withFile destination (#{const FILE_WRITE_ATTRIBUTES} .|. if directory then 0 else #{const GENERIC_WRITE}) #{const FILE_SHARE_READ}
   #{const OPEN_EXISTING} #{const FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT} $ \h -> do
   check destination (setTimes h (#{ptr BY_HANDLE_FILE_INFORMATION, ftCreationTime} info) nullPtr (#{ptr BY_HANDLE_FILE_INFORMATION, ftLastWriteTime} info))
   copySecurity source destination
   original <- #{peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes} info
   check destination (setAttributes p original)
   unless directory (check destination (flush h))

metadata :: FilePath -> IO B.ByteString
metadata path = inspect path $ \info -> do
 fields <- B.concat <$> mapM B.packCStringLen
  [(#{ptr BY_HANDLE_FILE_INFORMATION, dwFileAttributes} info, #{size DWORD})
  ,(#{ptr BY_HANDLE_FILE_INFORMATION, ftCreationTime} info, #{size FILETIME})
  ,(#{ptr BY_HANDLE_FILE_INFORMATION, ftLastWriteTime} info, #{size FILETIME})]
 security <- withSecurity path $ \_ _ _ sd -> alloca $ \text -> alloca $ \count -> bracket
  (check path (securityText sd #{const SDDL_REVISION_1} securityFlags text count) >> peek text)
  (void . freeLocal) (\p -> peek count >>= \n -> B.packCStringLen (castPtr p,fromIntegral n*2))
 content <- streams path $ \name -> do
  digest <- withRead (path++name) $ \h -> SHA.finalize <$> readChunks h (\ctx bytes -> pure (SHA.update ctx bytes)) SHA.init
  withCWStringLen (name ++ "\0") $ \(p,n) ->
   (<>digest) <$> B.packCStringLen (castPtr p,n * #{size WCHAR})
 kind <- #{peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes} info :: IO Word32
 when (null content && kind .&. #{const FILE_ATTRIBUTE_DIRECTORY} == 0) (fail "Missing file data stream")
 let result = fields <> security <> B.concat (sort content)
 when (B.length result>65536) (fail "Unsupported Windows metadata size")
 pure result

candidateJob :: Handle -> Bool -> IO ()
candidateJob job commit = if commit then zeroed #{size JOBOBJECT_EXTENDED_LIMIT_INFORMATION} $ \p -> do
 check "Read candidate ownership" (queryJob job #{const JobObjectExtendedLimitInformation} p #{size JOBOBJECT_EXTENDED_LIMIT_INFORMATION} nullPtr)
 flags <- #{peek JOBOBJECT_EXTENDED_LIMIT_INFORMATION, BasicLimitInformation.LimitFlags} p :: IO Word32
 #{poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, BasicLimitInformation.LimitFlags} p (flags .&. complement #{const JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE})
 check "Commit candidate ownership" (setJob job #{const JobObjectExtendedLimitInformation} p #{size JOBOBJECT_EXTENDED_LIMIT_INFORMATION})
 else do
  check "Terminate candidate" (terminateJob job 1)
  deadline <- (+30000000000) <$> getMonotonicTimeNSec
  let join = allocaBytes #{size JOBOBJECT_BASIC_ACCOUNTING_INFORMATION} $ \p -> do
       check "Join candidate" (queryJob job #{const JobObjectBasicAccountingInformation} p #{size JOBOBJECT_BASIC_ACCOUNTING_INFORMATION} nullPtr)
       active <- #{peek JOBOBJECT_BASIC_ACCOUNTING_INFORMATION, ActiveProcesses} p :: IO Word32
       when (active/=0) $ do
        now <- getMonotonicTimeNSec
        when (now>=deadline) (fail "Candidate process tree did not exit")
        threadDelay 10000 >> join
  join
