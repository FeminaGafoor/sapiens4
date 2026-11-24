module Sapiens.Config where

maxAttachments :: Int
maxAttachments = 8

voicePieceSeconds :: Int
voicePieceSeconds = 2

voiceMaxAttempts :: Int
voiceMaxAttempts = 5

voiceMaxBacklog :: Int
voiceMaxBacklog = 15

voiceClockMs :: Number
voiceClockMs = 1000.0

voiceRetryMs :: Number
voiceRetryMs = 1000.0

voiceTranscriptPollMs :: Number
voiceTranscriptPollMs = 1500.0
