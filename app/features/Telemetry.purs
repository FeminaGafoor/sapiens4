module Sapiens.Telemetry where

import Prelude
import Data.Argonaut.Core (Json)
import Sapiens.Data as D
import Sapiens.Environment (Environment, clientDetails)
import Sapiens.Domain as X
import Data.String as S
import Libs4.Browser as B

browserConsentKey :: String
browserConsentKey = "s4.telemetry.browser.enabled"

statusPollMillis :: Number
statusPollMillis = 60000.0

heartbeatDue :: Boolean -> Boolean -> Number -> Number -> Number -> Environment -> Boolean
heartbeatDue hostConsent browserConsent interval previous now env = hostConsent && deviceConsent browserConsent env && not env.hidden && interval > 0.0 && (previous == 0.0 || now-previous >= interval)

deviceConsent :: Boolean -> Environment -> Boolean
deviceConsent browserConsent env = env.native || browserConsent

requestMetadata :: Environment -> Json
requestMetadata env =
  let info = clientDetails env
  in D.json
    { client_kind:if env.native then "native" else if env.installed then "installed_webapp" else "browser"
    , client_device_family:info.family,client_os:S.toLower info.os }

metadata :: Boolean -> X.Location -> Number -> Environment -> Json
metadata remote location now env = D.merge (requestMetadata env) $ D.json
  { client_os_version_ua:(clientDetails env).version,client_user_agent:env.userAgent
  , ui_mode:if remote then "remote_control" else "local",ui_visible:not env.hidden
  , target_location:case location of
      X.Local -> "local"
      X.Remote -> "remote"
      _ -> "unknown"
  , occurred_at:B.timestamp now }
