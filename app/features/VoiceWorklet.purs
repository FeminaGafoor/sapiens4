module Sapiens.VoiceWorklet where

import Prelude
import Effect (Effect)

foreign import register :: String -> Effect Unit

main :: Effect Unit
main = register "s4-voice"
