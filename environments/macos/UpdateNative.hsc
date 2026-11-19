{-# LANGUAGE CApiFFI #-}
module UpdateNative where

#include <sys/stat.h>
#include <sys/mount.h>
#include <sys/xattr.h>
#include <sys/acl.h>
#include <sys/attr.h>
#include <copyfile.h>
#include <fcntl.h>
#include <limits.h>
#include <unistd.h>
#include <stdio.h>

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.List (sort)
import Foreign hiding (void)
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd(..))
import UpdateState (pathPresentWith)

foreign import capi safe "fcntl.h open" open :: CString -> CInt -> CUInt -> IO CInt
foreign import capi safe "fcntl.h fcntl" lock :: CInt -> CInt -> Ptr () -> IO CInt
foreign import capi safe "fcntl.h fcntl" control :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "close" close :: CInt -> IO CInt
foreign import capi safe "sys/stat.h lstat" stat :: CString -> Ptr () -> IO CInt
foreign import capi safe "sys/stat.h fstat" fstat :: CInt -> Ptr () -> IO CInt
foreign import capi safe "sys/mount.h statfs" statfs :: CString -> Ptr () -> IO CInt
foreign import ccall safe "fsync" fsync :: CInt -> IO CInt
foreign import capi safe "stdio.h renamex_np" renameExclusive :: CString -> CString -> CUInt -> IO CInt
foreign import ccall safe "rename" rename :: CString -> CString -> IO CInt
foreign import ccall safe "unlink" unlink :: CString -> IO CInt
foreign import capi safe "copyfile.h copyfile" copy :: CString -> CString -> Ptr () -> CUInt -> IO CInt
foreign import capi safe "unistd.h getattrlist" getAttributes :: CString -> Ptr () -> Ptr () -> CSize -> CULong -> IO CInt
foreign import capi safe "unistd.h setattrlist" attributes :: CString -> Ptr () -> Ptr () -> CSize -> CULong -> IO CInt
foreign import ccall safe "acl_get_file" acl :: CString -> CInt -> IO (Ptr ())
foreign import ccall safe "acl_to_text" aclText :: Ptr () -> Ptr CLong -> IO CString
foreign import ccall unsafe "acl_free" aclFree :: Ptr a -> IO CInt
foreign import ccall safe "listxattr" listAttrs :: CString -> CString -> CSize -> CInt -> IO CLong
foreign import ccall safe "getxattr" getAttr :: CString -> CString -> CString -> CSize -> CUInt -> CInt -> IO CLong

checked :: (Eq a, Num a) => String -> IO a -> IO a
checked = throwErrnoIfMinus1Retry
zeroed :: Int -> (Ptr () -> IO a) -> IO a
zeroed size action = allocaBytes size $ \p -> fillBytes p 0 size >> action p
withFD :: FilePath -> CInt -> (CInt -> IO a) -> IO a
withFD path flags = bracket (withCString path $ \p -> checked path (open p (flags .|. #{const O_CLOEXEC} .|. #{const O_NOFOLLOW}) 0o600)) (void . close)
withStat :: FilePath -> (Ptr () -> IO a) -> IO a
withStat path action = allocaBytes #{size struct stat} $ \s -> withCString path $ \p -> checked path (stat p s) >> action s

validateEntry :: FilePath -> IO ()
validateEntry path = withStat path $ \s -> do
 mode <- #{peek struct stat, st_mode} s :: IO CUShort
 links <- #{peek struct stat, st_nlink} s :: IO CUShort
 flags <- #{peek struct stat, st_flags} s :: IO CUInt
 let kind = mode .&. #{const S_IFMT}
 unless (kind `elem` [#{const S_IFREG},#{const S_IFDIR}] && (kind/= #{const S_IFREG} || links==1)
  && flags .&. #{const UF_IMMUTABLE | UF_APPEND | SF_IMMUTABLE | SF_APPEND} == 0) (fail ("Unsupported owned path: " ++ path))
 allocaBytes #{size struct statfs} $ \fs -> withCString path $ \p -> do
  void (checked path (statfs p fs))
  name <- peekCString (#{ptr struct statfs, f_fstypename} fs)
  unless (name=="apfs") (fail "Updates require APFS")

validatePath :: FilePath -> IO ()
validatePath path = pathPresent path >>= \exists -> unless exists (fail ("Missing owned path: " ++ path))

pathPresent :: FilePath -> IO Bool
pathPresent = pathPresentWith $ \path -> (validateEntry path >> pure True) `catch` \e ->
 if isDoesNotExistError e then pure False else throwIO e

actualName :: FilePath -> IO (Maybe FilePath)
actualName path = catchJust (guard . isDoesNotExistError) readName (const (pure Nothing))
 where
 readName = zeroed #{size struct attrlist} $ \request -> allocaBytes capacity $ \buffer -> do
  #{poke struct attrlist, bitmapcount} request (#{const ATTR_BIT_MAP_COUNT} :: CUShort)
  #{poke struct attrlist, commonattr} request (#{const ATTR_CMN_NAME} :: CUInt)
  withCString path $ \name -> void $ checked path (getAttributes name request buffer (fromIntegral capacity) #{const FSOPT_NOFOLLOW})
  size <- fromIntegral <$> (peek (castPtr buffer) :: IO CUInt)
  let ref = buffer `plusPtr` #{size uint32_t}
  offset <- fromIntegral <$> (#{peek attrreference_t, attr_dataoffset} ref :: IO CInt)
  count <- fromIntegral <$> (#{peek attrreference_t, attr_length} ref :: IO CUInt)
  unless (offset >= #{size attrreference_t} && count > 1 && #{size uint32_t} + offset + count <= size && size <= capacity) $
   fail ("Invalid filesystem name for " ++ path)
  Just <$> peekCStringLen (ref `plusPtr` offset,count-1)
 capacity = #{size uint32_t} + #{size attrreference_t} + #{const NAME_MAX} + 1

withRootIdentity :: FilePath -> (String -> IO a) -> IO a
withRootIdentity path action = validatePath path >> withFD path (#{const O_RDONLY} .|. #{const O_DIRECTORY}) (\fd ->
 allocaBytes #{size struct stat} $ \s -> do
  void (checked path (fstat fd s))
  device <- #{peek struct stat, st_dev} s :: IO CInt
  inode <- #{peek struct stat, st_ino} s :: IO Word64
  action (show [fromIntegral device,inode]))

newtype Lease = Lease CInt
acquireLease :: FilePath -> Bool -> IO Lease
acquireLease path exclusive = leaseWithMode path (if exclusive then 1 else 0)
leaseWithMode :: FilePath -> Int -> IO Lease
leaseWithMode path mode = mask $ \restore -> do
 fd <- withCString path $ \p -> checked path (open p (#{const O_RDWR | O_CREAT | O_CLOEXEC | O_NOFOLLOW}) 0o600)
 let acquire = do
      deadline <- (+15000000000) <$> getMonotonicTimeNSec
      let retry = do
           result <- setLock fd (mode/=0)
           when (result<0) $ do
            err <- getErrno
            now <- getMonotonicTimeNSec
            if mode/=2 && now<deadline && err `elem` [eACCES,eAGAIN,eINTR]
             then threadDelay 50000 >> retry else throwErrno "Installation lease"
      retry
 let validate = allocaBytes #{size struct stat} $ \info -> do
      void (checked path (fstat fd info))
      modeBits <- #{peek struct stat, st_mode} info :: IO Word16
      links <- #{peek struct stat, st_nlink} info :: IO Word16
      unless (modeBits .&. #{const S_IFMT} == #{const S_IFREG} && links == 1) $
       fail ("Unsupported lock file: " ++ path)
 (validate >> restore acquire) `onException` void (close fd)
 pure (Lease fd)
setLock :: CInt -> Bool -> IO CInt
setLock fd exclusive = zeroed #{size struct flock} $ \p -> do
 #{poke struct flock, l_whence} p (#{const SEEK_SET} :: CShort)
 #{poke struct flock, l_type} p (if exclusive then #{const F_WRLCK} else #{const F_RDLCK} :: CShort)
 lock fd #{const F_SETLK} p
releaseLease :: Lease -> IO ()
releaseLease (Lease fd) = void (close fd)
withLease :: FilePath -> Bool -> (Lease -> IO a) -> IO a
withLease path mode = bracket (acquireLease path mode) releaseLease
withIdleLease :: FilePath -> IO a -> IO a
withIdleLease path = bracket (leaseWithMode path 2) releaseLease . const
downgradeLease :: Lease -> IO ()
downgradeLease (Lease fd) = void (checked "Lease handoff" (setLock fd False))

flush :: FilePath -> Bool -> IO ()
flush path full = withFD path #{const O_RDONLY} $ \fd -> do
 void (checked path (fsync fd))
 when full (void (checked path (control fd #{const F_FULLFSYNC} 0)))
publish, publishNew :: FilePath -> FilePath -> B.ByteString -> IO ()
publish = publishWith True
publishNew = publishWith False
publishWith :: Bool -> FilePath -> FilePath -> B.ByteString -> IO ()
publishWith replace path temporary bytes = mask $ \restore -> do
 fd <- withCString temporary $ \p -> checked temporary (open p #{const O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC | O_NOFOLLOW} 0o600)
 let cleanup = withCString temporary (void . unlink)
 (do
   handle <- fdToHandle (Fd fd) `onException` void (close fd)
   bracket (pure handle) hClose $ \h -> do
    restore (B.hPut h bytes >> hFlush h)
    void (checked temporary (control fd #{const F_FULLFSYNC} 0))
   withCString temporary $ \t -> withCString path $ \p ->
    void (checked path (if replace then rename t p else renameExclusive t p #{const RENAME_EXCL}))
   flush (takeDirectory path) True) `onException` cleanup

copyOwned :: FilePath -> FilePath -> IO ()
copyOwned = copyWith #{const COPYFILE_ALL}
copyMetadata :: Bool -> FilePath -> FilePath -> IO ()
copyMetadata directory source destination = when directory (copyWith #{const COPYFILE_METADATA} source destination)
copyWith :: CUInt -> FilePath -> FilePath -> IO ()
copyWith flags source destination = do
 validatePath source
 withCString source $ \s -> withCString destination $ \d -> do
  void (checked destination (copy s d nullPtr (flags .|. #{const COPYFILE_NOFOLLOW})))
  withStat source $ \st -> zeroed #{size struct attrlist} $ \attrs -> do
   #{poke struct attrlist, bitmapcount} attrs (#{const ATTR_BIT_MAP_COUNT} :: CUShort)
   #{poke struct attrlist, commonattr} attrs (#{const ATTR_CMN_CRTIME} :: CUInt)
   void (checked destination (attributes d attrs (#{ptr struct stat, st_birthtimespec} st) #{size struct timespec} #{const FSOPT_NOFOLLOW}))
 withStat destination $ \st -> do
  mode <- #{peek struct stat, st_mode} st :: IO CUShort
  flush destination (mode .&. #{const S_IFMT} == #{const S_IFREG})

metadata :: FilePath -> IO B.ByteString
metadata path = do
 validatePath path
 withCString path $ \p -> withStat path $ \st -> do
  fields <- B.concat <$> mapM (\(offset,size) -> B.packCStringLen (st `plusPtr` offset,size))
   [(#{offset struct stat, st_mode},#{size mode_t}),(#{offset struct stat, st_uid},#{size uid_t})
   ,(#{offset struct stat, st_gid},#{size gid_t}),(#{offset struct stat, st_flags},#{size uint32_t})
   ,(#{offset struct stat, st_mtimespec},#{size struct timespec}),(#{offset struct stat, st_birthtimespec},#{size struct timespec})]
  permissions <- bracket (acl p #{const ACL_TYPE_EXTENDED}) (\a -> when (a/=nullPtr) (void (aclFree a))) $ \a ->
   if a==nullPtr then do
    err <- getErrno
    if err==eNOENT then sized B.empty else throwErrno path
   else alloca $ \count -> bracket (throwErrnoIfNull path (aclText a count)) (void . aclFree) $ \text ->
    peek count >>= \n -> B.packCStringLen (text,fromIntegral n) >>= sized
  names <- variable (\buffer size -> listAttrs p buffer size #{const XATTR_NOFOLLOW})
  extended <- forM (sort (filter (not . B.null) (BC.split '\0' names))) $ \name -> B.useAsCString name $ \key -> do
   value <- variable (\buffer size -> getAttr p key buffer size 0 #{const XATTR_NOFOLLOW}) >>= sized
   pure (name <> B.singleton 0 <> value)
  let prefix = L.fromStrict (fields <> permissions <> B.concat extended)
  mode <- #{peek struct stat, st_mode} st :: IO CUShort
  if mode .&. #{const S_IFMT} /= #{const S_IFREG} then pure (SHA.hashlazy prefix) else
   bracket (do
    fd <- withCString path $ \name -> checked path (open name #{const O_RDONLY | O_CLOEXEC | O_NOFOLLOW} 0)
    fdToHandle (Fd fd) `onException` void (close fd)) hClose $ \h ->
    L.hGetContents h >>= evaluate . SHA.hashlazy . (prefix <>)
 where
 sized bytes = with (fromIntegral (B.length bytes) :: CLong) $ \n ->
  (<> bytes) <$> B.packCStringLen (castPtr n,sizeOf (undefined :: CLong))
 variable readBytes = do
  count <- checked path (readBytes nullPtr 0)
  allocaBytes (fromIntegral count + 1) $ \buffer -> do
   actual <- checked path (readBytes buffer (fromIntegral count))
   unless (actual==count) (fail "Metadata changed during backup")
   B.packCStringLen (buffer,fromIntegral count)
