module Sapiens.Updates where

import Prelude
import Data.Argonaut.Core (toNumber)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Halogen.Subscription as HS
import Libs4.Browser as B
import Sapiens.Data as D

type Feed = { socket :: Maybe B.Socket, generation :: Int, revision :: Maybe Number
            , dirty :: Boolean, retryAt :: Number, backoff :: Number }
type Updates = { feed :: Ref.Ref Feed, wake :: HS.Listener Unit, events :: HS.Emitter Unit }

initial :: Int -> Feed
initial generation = {socket:Nothing,generation,revision:Nothing,dirty:false,retryAt:0.0,backoff:2000.0}

create :: Effect Updates
create = do
  feed <- Ref.new (initial 0)
  {listener,emitter} <- HS.create
  pure {feed,wake:listener,events:emitter}

stop :: Updates -> Effect Unit
stop updates = do
  feed <- Ref.read updates.feed
  Ref.write (initial (feed.generation + 1)) updates.feed
  traverse_ B.socketClose feed.socket

connect :: Updates -> String -> Effect Unit
connect updates address = do
  feed <- Ref.read updates.feed
  time <- B.now
  when (not (isJust feed.socket) && time >= feed.retryAt) do
    let generation = feed.generation + 1
        current f = do
          s <- Ref.read updates.feed
          when (s.generation == generation) (f s)
        changed raw = traverse_ (\revision -> current \s -> when (s.revision /= Just revision) do
          Ref.write (s {revision=Just revision,dirty=true,backoff=2000.0}) updates.feed
          HS.notify updates.wake unit) (toNumber (D.parse raw))
        closed = current \s -> do
          now <- B.now
          Ref.write (s {socket=Nothing,revision=Nothing,dirty=false,retryAt=now+s.backoff,backoff=min 30000.0 (s.backoff*2.0)}) updates.feed
          when (isJust s.revision) (HS.notify updates.wake unit)
    Ref.write (feed {generation=generation}) updates.feed
    Exception.catchException (const closed) do
      socket <- B.openSocket address (pure unit) changed (const closed)
      Ref.modify_ (_ {socket=Just socket}) updates.feed

beginRefresh :: Updates -> Effect (Maybe Int)
beginRefresh updates = do
  feed <- Ref.read updates.feed
  Ref.modify_ (_ {dirty=false}) updates.feed
  pure (feed.generation <$ feed.revision)

acceptRevision :: Updates -> Maybe Int -> Number -> Effect Unit
acceptRevision updates source revision = Ref.modify_ consume updates.feed
  where
  consume feed | Just feed.generation == source, Just latest <- feed.revision, latest <= revision = feed {dirty=false}
  consume feed = feed
