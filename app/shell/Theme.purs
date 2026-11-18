module Sapiens.Theme where

import Prelude
import Data.Array as A
import Effect (Effect)
import Sapiens.BrowserNative as N
import Sapiens.Data as D

main :: Effect Unit
main = do
  environment <- N.environment
  setup <- N.rootAttribute "data-setup"
  saved <- if setup == "true" then pure "" else N.storageRead "sapi-color-theme"
  let theme = if A.elem saved ["dark", "light"] then saved else if D.b "dark" environment then "dark" else "light"
  N.setAttribute "html" "data-theme" theme
