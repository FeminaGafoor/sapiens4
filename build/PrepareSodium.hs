-- runghc build/PrepareSodium.hs libsodium-1.0.20.tar.gz NEW_OUTPUT_DIRECTORY
-- Requires Autoconf, Automake, Libtool, tar and gzip. Normal builds use the archive.
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Data.Time.Clock.POSIX

version, upstreamHash :: String
version = "libsodium-1.0.20-remote-1"
upstreamHash = "ebb65ef6ca439333c2bb41a0c1990587288da07f6c7fd07cb3a18cc18d30ce19"

main :: IO ()
main = do
 args <- getArgs
 (input,output) <- case args of
  [a,b] -> (,) <$> makeAbsolute a <*> makeAbsolute b
  _ -> fail "Usage: runghc build/PrepareSodium.hs UPSTREAM_ARCHIVE NEW_OUTPUT_DIRECTORY"
 sha <- findExecutable "sha256sum"
 digest <- case sha of
  Just tool -> readProcess tool [input] ""
  Nothing -> readProcess "shasum" ["-a","256",input] ""
 unless (take 64 digest == upstreamHash) $ fail "Unexpected upstream libsodium archive"
 exists <- doesPathExist output
 when exists $ fail "Output directory must not exist"
 createDirectory output
 callProcess "tar" ["-xzf",input,"-C",output]
 let source = output </> "libsodium-1.0.20"
     target = output </> version
 createDirectory target
 paths <- files source
 forM_ (filter keep paths) $ \path -> do
  createDirectoryIfMissing True (takeDirectory (target </> path))
  copyFile (source </> path) (target </> path)
 change (target </> "configure.ac") $ unlines . filter (\line -> not $ any (`isInfixOf` line) ["builds/Makefile","contrib/Makefile","dist-build/Makefile"]) . lines
 writeFile (target </> "Makefile.am") $ unlines
  ["ACLOCAL_AMFLAGS = -I m4", "EXTRA_DIST = LICENSE AUTHORS README.sapiens"
  ,"SUBDIRS = src test", "pkgconfigdir = $(libdir)/pkgconfig"
  ,"pkgconfig_DATA = @PACKAGE_NAME@.pc", "DISTCLEANFILES = $(pkgconfig_DATA)"]
 change (target </> "src/libsodium/Makefile.am") libraryMakefile
 change (target </> "src/libsodium/sodium/core.c") $ unlines . filter (\line -> not $ any (`isInfixOf` line) unusedInitializers) . lines
 writeFile (target </> "test/default/Makefile.am") testMakefile
 writeFile (target </> "README.sapiens") $ unlines
  ["Sapiens4 remote crypto subset of libsodium 1.0.20."
  ,"Upstream archive SHA-256: " ++ upstreamHash
  ,"Includes NaCl box, X25519, XSalsa20, Poly1305, randomness and utilities."
  ,"Primitive implementations are unchanged. Initialization excludes unused algorithms."
  ,"Regenerate with build/PrepareSodium.hs in the Sapiens4 repository."
  ,"Build: ./configure --disable-shared --enable-static && make && make check"]
 withCurrentDirectory target $ callProcess "autoreconf" ["--force","--install"]
 generated <- files target
 let packaged = filter (\p -> not ("autom4te.cache/" `isPrefixOf` p) && not ("~" `isSuffixOf` p)) generated
 forM_ packaged $ \p -> setModificationTime (target </> p) (posixSecondsToUTCTime 1720000000)
 writeFile (output </> "files.list") $ unlines $ map (version </>) packaged
 tarVersion <- readProcess "tar" ["--version"] ""
 let owner = if "bsdtar" `isInfixOf` tarVersion then ["--uid","0","--gid","0","--uname","root","--gname","root"] else ["--owner=0","--group=0"]
 withCurrentDirectory output $ do
  callProcess "tar" (["--format=ustar"] ++ owner ++ ["-cf",version<>".tar","-T","files.list"])
  callProcess "gzip" ["-n","-9",version<>".tar"]
 putStrLn (output </> version<>".tar.gz")

files :: FilePath -> IO [FilePath]
files root = sort . concat <$> (listDirectory root >>= mapM (\name -> do
 directory <- doesDirectoryExist (root </> name)
 if directory then map (name </>) <$> files (root </> name) else pure [name]))

change :: FilePath -> (String -> String) -> IO ()
change path f = B.readFile path >>= B.writeFile path . B.pack . f . B.unpack

tests :: [String]
tests = words "box box2 box7 box8 box_easy box_easy2 box_seed onetimeauth onetimeauth2 onetimeauth7 scalarmult scalarmult2 scalarmult5 scalarmult6 scalarmult7 scalarmult8 secretbox secretbox2 secretbox7 secretbox8 secretbox_easy secretbox_easy2 randombytes sodium_core sodium_utils sodium_version verify1 chacha20"

keep :: FilePath -> Bool
keep p
 | "src/libsodium/" `isPrefixOf` p = keepLibrary (drop (length "src/libsodium/") p)
 | "test/default/" `isPrefixOf` p = takeFileName p `elem` ([t<>ext | t <- tests, ext <- [".c",".exp"]] ++ ["cmptest.h","wasi-test-wrapper.sh","pre.js.inc"])
 | otherwise = p `elem` ["LICENSE","AUTHORS","configure.ac","libsodium.pc.in","libsodium-uninstalled.pc.in","src/Makefile.am","test/Makefile.am","test/quirks/quirks.h"] || "m4/" `isPrefixOf` p

keepLibrary :: FilePath -> Bool
keepLibrary p = "include/" `isPrefixOf` p || takeExtension p == ".h" || p == "Makefile.am" || p `elem` wrappers || any (`isPrefixOf` p) primitives
 where
 wrappers = words "crypto_box/crypto_box.c crypto_box/crypto_box_easy.c crypto_onetimeauth/crypto_onetimeauth.c crypto_scalarmult/crypto_scalarmult.c crypto_secretbox/crypto_secretbox.c crypto_secretbox/crypto_secretbox_easy.c crypto_stream/crypto_stream.c sodium/core.c sodium/runtime.c sodium/utils.c sodium/codecs.c sodium/version.c"
 primitives = words "crypto_box/curve25519xsalsa20poly1305/ crypto_core/ed25519/ref10/ crypto_core/hsalsa20/ crypto_core/salsa/ crypto_hash/sha512/ crypto_onetimeauth/poly1305/ crypto_scalarmult/curve25519/ crypto_secretbox/xsalsa20poly1305/ crypto_stream/chacha20/ crypto_stream/salsa20/ crypto_stream/xsalsa20/ crypto_verify/ randombytes/"

unusedInitializers :: [String]
unusedInitializers = words "_crypto_pwhash_argon2_pick_best_implementation(); _crypto_generichash_blake2b_pick_best_implementation(); _crypto_aead_aegis128l_pick_best_implementation(); _crypto_aead_aegis256_pick_best_implementation();"

-- Preserve upstream architecture conditionals and per-implementation compiler flags.
libraryMakefile :: String -> String
libraryMakefile = unlines . concatMap rewrite . logicalLines . lines
 where
 rewrite line
  | any (`isPrefixOf` line) ["libaesni_la_","libarmcrypto_la_","libsse41_la_","libavx512f_la_"] = []
  | "libsodium_la_LIBADD =" `isPrefixOf` line = ["libsodium_la_LIBADD = libsse2.la libssse3.la libavx2.la"]
  | take 2 (words line) == ["noinst_LTLIBRARIES","="] = ["noinst_LTLIBRARIES = libsse2.la libssse3.la libavx2.la"]
  | "_SOURCES " `isInfixOf` line || "noinst_HEADERS " `isPrefixOf` line = case words line of
     name:op:paths -> case filter keepLibrary paths of
      [] -> []
      kept -> [unwords (name:op:kept)]
     _ -> [line]
  | otherwise = [line]

logicalLines :: [String] -> [String]
logicalLines [] = []
logicalLines (line:rest)
 | "\\" `isSuffixOf` line = case logicalLines rest of
    next:more -> (init line <> next):more
    [] -> [init line]
 | otherwise = line : logicalLines rest

testMakefile :: String
testMakefile = unlines $
 ["AM_CPPFLAGS = -I$(top_srcdir)/src/libsodium/include -I$(top_builddir)/src/libsodium/include -I$(top_srcdir)/src/libsodium/include/sodium -I$(top_builddir)/src/libsodium/include/sodium -I$(top_srcdir)/test/quirks -DTEST_SRCDIR=\\\"$(srcdir)\\\""
 ,"check_PROGRAMS = " ++ unwords tests, "TESTS = $(check_PROGRAMS)"
 ,"LDADD = $(top_builddir)/src/libsodium/libsodium.la"
 ,"EXTRA_DIST = cmptest.h wasi-test-wrapper.sh pre.js.inc " ++ unwords (map (<>".exp") tests)
 ,"if WASI", "LOG_COMPILER = ./wasi-test-wrapper.sh", "endif"]
 ++ [t<>"_SOURCES = "<>t<>".c cmptest.h" | t <- tests]
