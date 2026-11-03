{-# LANGUAGE OverloadedStrings #-}
-- | Voice messages. Audio streams in as short pieces (about 2 s each) and is cut
-- into voice messages of up to one minute. The decisions are pure functions;
-- 'receivePiece' is the adapter that reads and writes files.
module Voice
 ( Pcm, pcmBytes, pcmFromWav, wavFromPcm
 , Progress(..), Cut(..), PiecePlan(..), startProgress, planPiece
 , receivePiece
 ) where

import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import Types
import qualified Store as DB

sampleRate, bytesPerSecond, pieceLimit, messageLimit, minimumTail, maxParts :: Int
sampleRate = 16000
bytesPerSecond = sampleRate * 2        -- 16-bit mono
pieceLimit = 5 * bytesPerSecond        -- one streamed piece: at most 5 s
messageLimit = 60 * bytesPerSecond     -- one voice message: at most 1 minute
minimumTail = bytesPerSecond `div` 2   -- a final scrap under 0.5 s is dropped
maxParts = 60                          -- recordings stop at 60 minutes

-- | Raw 16 kHz mono 16-bit audio. Only 'pcmFromWav' can build one, so a 'Pcm'
-- is always in the format Whisper expects.
newtype Pcm = Pcm BS.ByteString

pcmBytes :: Pcm -> BS.ByteString
pcmBytes (Pcm bytes) = bytes

pcmFromWav :: BS.ByteString -> Either Text Pcm
pcmFromWav wav
 | BS.length wav < 44 || tag 0 /= "RIFF" || tag 8 /= "WAVE" || tag 12 /= "fmt " = Left "Voice audio must be a WAV file"
 | word16 20 /= 1 || word16 22 /= 1 || word32 24 /= sampleRate || word16 34 /= 16 = Left "Voice audio must be 16 kHz mono 16-bit PCM"
 | tag 36 /= "data" || word32 40 /= BS.length body = Left "Voice audio is incomplete"
 | BS.null body || odd (BS.length body) || BS.length body > pieceLimit = Left "Voice pieces must hold up to 5 seconds of audio"
 | otherwise = Right (Pcm body)
 where
 body = BS.drop 44 wav
 tag i = BS.take 4 (BS.drop i wav)
 byte i = fromIntegral (BS.index wav i) :: Int
 word16 i = byte i + 256 * byte (i + 1)
 word32 i = word16 i + 65536 * word16 (i + 2)

wavFromPcm :: BS.ByteString -> BS.ByteString
wavFromPcm pcm = BL.toStrict $ BB.toLazyByteString $ mconcat
 [ BB.byteString "RIFF", w32 (36 + size), BB.byteString "WAVEfmt ", w32 16, w16 1, w16 1
 , w32 sampleRate, w32 bytesPerSecond, w16 2, w16 16, BB.byteString "data", w32 size, BB.byteString pcm ]
 where
 size = BS.length pcm
 w32, w16 :: Int -> BB.Builder
 w32 = BB.word32LE . fromIntegral
 w16 = BB.word16LE . fromIntegral

-- | Where a recording stands: the next piece we expect, the next voice message
-- number, how many audio bytes wait for the next cut, and whether it has ended.
data Progress = Progress { nextPiece :: Int, nextPart :: Int, buffered :: Int, finished :: Bool } deriving (Eq, Show)

startProgress :: Progress
startProgress = Progress 0 0 0 False

-- | A finished voice message: its number and how many buffered bytes it takes.
data Cut = Cut { cutPart :: Int, cutBytes :: Int } deriving (Eq, Show)

data PiecePlan = DuplicatePiece | AppendPiece Progress [Cut] deriving (Eq, Show)

-- | Decide what one incoming piece does. Pure: no files, no clock.
planPiece :: Progress -> Int -> Bool -> Int -> Either Text PiecePlan
planPiece progress index final size
 | index < nextPiece progress = Right DuplicatePiece
 | finished progress = Left "This recording is already finished"
 | index > nextPiece progress = Left "Voice piece arrived out of order; resend from the missing piece"
 | nextPart progress + length cuts > maxParts = Left "Recordings are limited to 60 minutes"
 | otherwise = Right (AppendPiece (Progress (index + 1) (nextPart progress + length cuts) remaining final) cuts)
 where
 total = buffered progress + size
 whole = total `div` messageLimit
 rest = total `mod` messageLimit
 cuts = [Cut (nextPart progress + k) messageLimit | k <- [0 .. whole - 1]]
     ++ [Cut (nextPart progress + whole) rest | final, rest >= minimumTail]
 remaining = if final then 0 else rest

-- | The adapter: validate the request, apply the plan, save any finished voice messages.
receivePiece :: Service -> Text -> Value -> IO Value
receivePiece s a d = do
 let recording = at d "recording"
     final = get d "final" == Bool True
 require (T.length recording == 36 && T.all (\c -> isHexDigit c || c == '-') recording) 400 "Invalid voice recording id"
 index <- case get d "index" of
  Number n | n >= 0 && n <= 100000 && n == fromIntegral (round n :: Int) -> pure (round n)
  _ -> failure 400 "Voice piece index must be a whole number"
 require (case get d "data" of String t -> T.length t <= 300000; _ -> False) 413 "Voice pieces must be 5 seconds or shorter"
 wav <- either (const (failure 400 "Invalid voice data")) pure (B64.decode (TE.encodeUtf8 (at d "data")))
 pcm <- either (failure 400) pure (pcmFromWav wav)
 let dir = serviceRoot s </> "uploads" </> T.unpack a </> "voice" </> T.unpack recording
     bufferFile = dir </> "buffer.pcm"
     progressFile = dir </> "progress.json"
 createDirectoryIfMissing True dir
 progress <- readProgress progressFile
 case planPiece progress index final (BS.length (pcmBytes pcm)) of
  Left problem -> failure 409 problem
  Right DuplicatePiece -> pure (reply recording True [])
  Right (AppendPiece next cuts) -> do
   waiting <- readIfPresent bufferFile
   let audio = BS.take (buffered progress) waiting <> pcmBytes pcm
   (rows, leftover) <- foldM (saveCut s a recording) ([], audio) cuts
   atomicBytes bufferFile (BL.fromStrict leftover)
   atomicBytes progressFile (encode (progressJSON next))
   pure (reply recording False (reverse rows))
 where
 reply recording duplicate rows = obj [("recording",str recording),("duplicate",Bool duplicate),("messages",arr rows)]

saveCut :: Service -> Text -> Text -> ([Value], BS.ByteString) -> Cut -> IO ([Value], BS.ByteString)
saveCut s a recording (rows, audio) cut = do
 let (part, rest) = BS.splitAt (cutBytes cut) audio
     name = "voice-" <> T.pack (show (cutPart cut + 1)) <> ".wav"
 i <- newId
 let path = serviceRoot s </> "uploads" </> T.unpack a </> T.unpack i </> T.unpack name
 atomicBytes path (BL.fromStrict (wavFromPcm part))
 let row = obj [("id",str i),("agent",str a),("kind",str "voice"),("name",str name),("value",str (T.pack path))
               ,("size",num (BS.length part + 44)),("recording",str recording),("part",num (cutPart cut))
               ,("duration_ms",num (BS.length part * 1000 `div` bytesPerSecond)),("transcript_status",str "pending")]
 DB.saveAttachment s row
 pure (row : rows, rest)

progressJSON :: Progress -> Value
progressJSON p = obj [("next_piece",num (nextPiece p)),("next_part",num (nextPart p)),("buffered",num (buffered p)),("finished",Bool (finished p))]

readProgress :: FilePath -> IO Progress
readProgress path = do
 exists <- doesFileExist path
 if not exists then pure startProgress else do
  value <- maybe (failure 500 "Voice recording state is damaged") pure . decode . BL.fromStrict =<< BS.readFile path
  pure (Progress (whole value "next_piece") (whole value "next_part") (whole value "buffered") (get value "finished" == Bool True))
 where whole v k = case get v k of Number n -> round n; _ -> 0

readIfPresent :: FilePath -> IO BS.ByteString
readIfPresent path = doesFileExist path >>= \exists -> if exists then BS.readFile path else pure BS.empty