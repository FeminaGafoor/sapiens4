module Sapiens.Voice where

import Prelude
import Control.Promise (toAffE)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Halogen as H
import Halogen.Subscription as HS
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Connections as C
import Sapiens.Data as D
import Sapiens.Effects as E
import Sapiens.Model as M

-- | Seconds of audio per streamed piece. The host joins pieces and cuts one-minute voice messages.
pieceSeconds :: Int
pieceSeconds = 2

-- | A piece that keeps failing to upload is retried this many times before the recording is dropped.
maxAttempts :: Int
maxAttempts = 5

-- | Recording, ordered upload and stopping. Returns true when it handled the action.
handle :: C.Runtime -> E.Dispatch -> Action -> E.UI Boolean
handle runtime dispatch action = case action of
  VoiceStart -> do
    m <- H.get
    when (isNothing m.voice && m.selected /= "") do
      { emitter, listener } <- liftEffect HS.create
      subscription <- H.subscribe (VoicePiece <$> emitter)
      H.modify_ _ { voice = Just { owner: m.selected, path: M.ownerPath m <> "/voice", epoch: m.epoch, recorder: Nothing, subscription
                                 , queue: [], sending: false, attempts: 0, pieces: 0, stopping: false } }
      E.attempt (toAffE $ B.voiceStart pieceSeconds (HS.notify listener)) case _ of
        Left err -> do
          H.unsubscribe subscription
          H.modify_ _ { voice = Nothing }
          E.notify ("Microphone unavailable: " <> message err)
        Right recorder -> H.gets _.voice >>= case _ of
          Just v | v.subscription == subscription -> do
            H.modify_ _ { voice = Just v { recorder = Just recorder } }
            when v.stopping $ stopRecorder recorder
          _ -> stopRecorder recorder
    pure true
  VoiceStop -> do
    H.gets _.voice >>= traverse_ \v -> do
      H.modify_ _ { voice = Just v { stopping = true } }
      traverse_ stopRecorder v.recorder
    pure true
  VoicePiece piece -> do
    H.modify_ \m -> m { voice = map (\v -> v { queue = A.snoc v.queue piece }) m.voice }
    drain
    pure true
  _ -> pure false
  where
  stopRecorder recorder = E.attempt (toAffE $ B.voiceStop recorder) (const (pure unit))
  drain = H.get >>= \m -> traverse_ (step m) m.voice
  step m v
    | m.epoch /= v.epoch = abandon v
    | v.sending = pure unit
    | otherwise = traverse_ (upload m v) (A.head v.queue)
  upload m v piece = do
    H.modify_ _ { voice = Just v { sending = true } }
    E.attempt (E.api runtime m v.path "POST" piece) case _ of
      Right _
        | D.b "final" piece -> finish v
        | otherwise -> do
            H.modify_ \s -> s { voice = map (\w -> w { queue = A.drop 1 w.queue, sending = false, attempts = 0, pieces = w.pieces + 1 }) s.voice }
            drain
      Left err
        | v.attempts + 1 >= maxAttempts -> do
            abandon v
            E.notify ("Voice message could not be sent: " <> message err)
        | otherwise -> do
            H.modify_ \s -> s { voice = map (\w -> w { sending = false, attempts = w.attempts + 1 }) s.voice }
            E.later 1000.0 drain
  finish v = do
    H.unsubscribe v.subscription
    H.modify_ _ { voice = Nothing }
    dispatch Poll
  abandon v = do
    traverse_ stopRecorder v.recorder
    H.unsubscribe v.subscription
    H.modify_ _ { voice = Nothing }
    