module HostUpdateProtocol
 ( HostCommand(..), renderHostCommand, parseHostCommand, hostStopLine
 , HostReply(..), renderHostReply, parseHostReply
 ) where

import Data.List (find)

data HostCommand = CheckBusy | Quiesce | Resume | PrepareReady | Activate
 deriving (Enum, Bounded)

renderHostCommand :: HostCommand -> String
renderHostCommand command = case command of
 CheckBusy -> "check-busy"
 Quiesce -> "quiesce"
 Resume -> "resume"
 PrepareReady -> "prepare-ready"
 Activate -> "activate"

parseHostCommand :: String -> Maybe HostCommand
parseHostCommand verb = find ((== verb) . renderHostCommand) [minBound .. maxBound]

hostStopLine :: String
hostStopLine = "stop"

data HostReply = Quiet | Busy | Resumed | Prepared | Active | CommandError
 deriving (Eq, Enum, Bounded)

renderHostReply :: HostReply -> String
renderHostReply reply = case reply of
 Quiet -> "quiet"
 Busy -> "busy"
 Resumed -> "resumed"
 Prepared -> "prepared"
 Active -> "active"
 CommandError -> "error"

parseHostReply :: String -> Maybe HostReply
parseHostReply line = find ((== line) . renderHostReply) [minBound .. maxBound]
