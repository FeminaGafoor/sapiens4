module Sapiens.Telemetry where

import Prelude
import Data.Argonaut.Core (Json)
import Sapiens.Data as D
import Sapiens.Environment (Environment, clientDetails)
import Libs4.Browser as B

browserConsentKey :: String
browserConsentKey = "s4.telemetry.browser.enabled"

statusPollMillis :: Number
statusPollMillis = 60000.0

heartbeatDue :: Boolean -> Boolean -> Number -> Number -> Number -> Environment -> Boolean
heartbeatDue hostConsent browserConsent interval previous now env = hostConsent && (env.native || browserConsent) && not env.hidden && interval > 0.0 && (previous == 0.0 || now-previous >= interval)

metadata :: Boolean -> Number -> Environment -> Json
metadata remote now env =
  let info = clientDetails env
  in D.json
    { client_user_agent:env.userAgent,client_kind:if env.native then "native" else if env.installed then "installed_webapp" else "browser"
    , client_device_family:info.family,client_os:info.os,client_os_version_ua:info.version,client_touch:env.touch
    , ui_mode:if remote then "remote_control" else "local",ui_visible:not env.hidden,ui_width:env.width,ui_height:env.height
    , occurred_at:B.timestamp now }
