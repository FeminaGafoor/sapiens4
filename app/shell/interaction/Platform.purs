module Sapiens.Platform where

import Prelude
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (either)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Libs4.Browser as B
import Sapiens.Environment as E

environment :: Effect E.Environment
environment = B.environment >>= either (throwException <<< error <<< printJsonDecodeError) pure <<< E.decode
