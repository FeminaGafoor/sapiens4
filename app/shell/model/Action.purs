module Sapiens.Action where

import Sapiens.Form as F
import Sapiens.Browser as Browser
import Data.Argonaut.Core (Json)
import Sapiens.Domain as X
import Sapiens.Model (DialogKind)
import Sapiens.Navigation (Activity, Scope, Pane, Period)
import Web.Event.Event (Event)

data Action
  = Initialize
  | LoadRegistry
  | Finalize
  | Poll
  | Select String
  | SetScope Scope
  | Navigate Activity
  | DefaultWork
  | SetPeriod Period
  | Search String
  | ToggleSearch
  | TogglePane Pane
  | Home
  | Draft String
  | MessageKey Event
  | Send
  | RetryDocument String
  | ToggleExpanded String
  | Clarify X.Workload
  | Menu String
  | CloseMenu
  | OpenDialog DialogKind F.Context
  | CloseDialog
  | Field F.Field String
  | CheckField String Boolean
  | SubmitDialog
  | DialogCommand String Json
  | Copy String String
  | Scroll
  | UserScroll
  | Resize
  | Global Browser.GlobalEvent
  | GlobalEvent String Event
  | DrainNative
  | NativeReport Browser.NativeReport
  | MentionChoice Int
  | MentionCursor
  | RefreshTheme
  | Theme
  | Persist
  | EnterCorpora String
  | Lifecycle String
  | ShowArchived Boolean
  | Mutate String String Json Success
  | AttachmentKind String
  | RemoveAttachment String
  | Browser String Json
  | Address String
  | OpenAddress
  | EditAddress
  | OpenFile
  | PrepareDraft String String
  | Feedback
  | Pair
  | ScanQR
  | ScanFrame Int
  | UploadQR
  | StopScan
  | HostPairPoll Int
  | Install
  | Submit Event Action
  | Link Event Action
  | OpenLink String
  | NoteAnchor String

data Success = Refresh | CloseAndRefresh String | SavedPrompt String
