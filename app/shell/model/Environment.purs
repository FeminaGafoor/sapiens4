module Sapiens.Environment where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either)
import Foreign.Object (Object)
import Sapiens.Data as D

type Environment =
  { origin :: String, href :: String, path :: String, hash :: String, search :: String
  , contentType :: String, body :: Object String, userAgent :: String, platform :: String
  , touchPoints :: Number, installed :: Boolean, dark :: Boolean, touch :: Boolean
  , width :: Number, height :: Number, orientation :: String
  , viewportHeight :: Number, viewportScale :: Number, hidden :: Boolean
  , secure :: Boolean, native :: Boolean, online :: Boolean
  }

decode :: Json -> Either JsonDecodeError Environment
decode = decodeJson

clientDetails :: Environment -> { name :: String, family :: String, os :: String, version :: String }
clientDetails env =
  let ua = env.userAgent
      ipad = D.matches "iPad" ua || (D.matches "Macintosh" ua && env.touchPoints > 1.0)
      phone = D.matches "iPhone|Android.*Mobile" ua
      name = if ipad then "iPad" else if D.matches "iPhone" ua then "iPhone" else if D.matches "Android" ua then "Android device" else if D.matches "Windows" ua then "Windows PC" else if D.matches "Macintosh" ua then "Mac" else if D.matches "Linux" ua then "Linux device" else "Remote device"
      os = case name of
        "iPad" -> "iPadOS"
        "iPhone" -> "iOS"
        "Android device" -> "Android"
        "Windows PC" -> "Windows"
        "Mac" -> "macOS"
        "Linux device" -> "Linux"
        _ -> "unknown"
      pattern = "(?:CPU (?:iPhone )?OS|iPhone OS|Android|Windows NT|Mac OS X) ([0-9_.]+)"
  in {name,os,family:if os == "unknown" then "unknown" else if ipad || (D.matches "Android" ua && not phone) then "tablet" else if phone then "phone" else "desktop"
     ,version:if D.matches pattern ua then D.replace "_" "." (D.replace ("^.*?" <> pattern <> ".*$") "$1" ua) else ""}
