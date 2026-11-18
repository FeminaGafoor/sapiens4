import io.CryptoWindow
import io.Relay

open Lean Spec
namespace Spec.SystemJobs

def requireAnOpenBrowserForRemoteCrypto : IO Unit := withRelay fun relay admin =>
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let selected := json% {relay: $(relay), token: $(admin)}
    IO.ofExcept <| fieldsMatch (← host.api "GET" "/api/host/crypto") (json% {browser_crypto: true, connected: false})
    discard <| host.api "POST" "/api/remote/pair" selected 503
    let denied ← request host.url "GET" "/api/host/crypto" none #["Origin: https://untrusted.invalid"]
    IO.ofExcept <| requireThat "foreign origin accessed browser crypto" (denied.status == 403)
    let upgrade := #["Connection: Upgrade", "Upgrade: websocket", "Sec-WebSocket-Version: 13",
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="]
    for headers in [upgrade, upgrade.push "Origin: https://untrusted.invalid", upgrade.push "Origin: null"] do
      let denied ← request host.url "GET" "/api/host/crypto" none headers
      IO.ofExcept <| requireThat "untrusted WebSocket acquired browser crypto" (denied.status == 403)
    let qr ← withCryptoWindow host (do
      let pairing ← host.api "POST" "/api/remote/pair" selected 201
      IO.ofExcept <| pairing.getObjVal? "qr") true
    discard <| untilSome "closed crypto window" do
      let state ← host.api "GET" "/api/host/crypto"
      return if state.getObjValD "connected" == toJson false then some () else none
    discard <| host.api "POST" "/api/host/pair" qr 503
    withCryptoWindow host do
      IO.ofExcept <| fieldsMatch (← host.api "GET" "/api/host/crypto") (json% {connected: true})
      discard <| host.api "POST" "/api/remote/revoke" (json% {})

end Spec.SystemJobs
