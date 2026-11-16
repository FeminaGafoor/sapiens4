import support.Relay

open Lean
namespace Spec.Founder

def retainControlOverScopeAndSuspendedWork : IO Unit := IO.FS.withTempDir fun directory => do
  let metadata := fun (host : Host) => host.api "GET" "/api/corpora"
  let (active, pauseCommand) ← withHost directory fun host => do
    let chief ← host.chief
    let colleague ← host.agent "Planner"
    let initial ← metadata host
    let revision ← IO.ofExcept <| initial.getObjVal? "revision"
    let change := json% {op: "corpora_update", name: "Studio", scope: "Private review scope", revision: $(revision)}
    let refused ← request host.url "POST" ("/api/agents/" ++ colleague ++ "/control") change
    IO.ofExcept <| verifyDenial "only the chief can change scope" 403 refused initial (← metadata host) []
    let updated ← host.control chief change
    IO.ofExcept <| fieldsMatch updated (json% {name: "Studio", scope: "Private review scope"})
    let revision ← IO.ofExcept <| updated.getObjVal? "revision"
    let pauseCommand := json% {action: "pause", revision: $(revision), operation: "8aa103a9-56bb-492d-aa30-1ae30f837071"}
    let paused ← host.api "PUT" "/api/corpora/lifecycle" pauseCommand
    let turns := do IO.ofExcept <| (← host.state).getObjVal? "turns"
    let before ← turns
    let denied ← request host.url "POST" ("/api/agents/" ++ chief ++ "/messages") (json% {text: "Start budget review"})
    IO.ofExcept <| verifyDenial "suspended work" 409 denied before (← turns) ["Private review scope"]
    let revision ← IO.ofExcept <| paused.getObjVal? "revision"
    let active ← host.api "PUT" "/api/corpora/lifecycle"
      (json% {action: "activate", revision: $(revision), operation: "0f92546f-143a-41d5-be6a-80a84a10e7b9"})
    return (active, pauseCommand)
  withHost directory fun host => do
    let replay ← host.api "PUT" "/api/corpora/lifecycle" pauseCommand
    let persisted ← metadata host
    let state ← host.state
    IO.ofExcept do
      fieldsMatch replay (json% {status: "active", revision: $(← active.getObjVal? "revision")})
      fieldsMatch persisted (json% {status: "active", scope: "Private review scope", revision: $(← active.getObjVal? "revision")})
      fieldsMatch (← state.getObjVal? "pulse") (json% {running: true})

def approveOneDeviceThenRevokeItsMailbox : IO Unit := withRelay fun relay admin =>
  IO.FS.withTempDir fun authorityDir => IO.FS.withTempDir fun deviceDir => do
    withHost authorityDir fun authority => do
      let marker := "UnlistedReviewer"
      discard <| authority.agent marker
      let identity ← IO.ofExcept <| stringField (← authority.api "GET" "/api/corpora") "id"
      let invitation ← authority.api "POST" "/api/remote/pair" (json% {relay: $(relay), token: $(admin)}) 201
      let qr ← IO.ofExcept <| invitation.getObjVal? "qr"
      let mailbox ← IO.ofExcept <| stringField qr "room"
      let token ← IO.ofExcept <| stringField qr "token"
      let secret ← IO.ofExcept <| stringField qr "secret"
      let discoveryPath := "/api/corpora/" ++ (← IO.ofExcept <| stringField qr "public_id")
      let discover ← request relay "GET" discoveryPath
      IO.ofExcept do
        requireThat "discovery should be available" (discover.status == 200)
        let publicInfo ← Json.parse discover.body
        let keys := (← publicInfo.getObj?).toList.map Prod.fst
        requireThat "unexpected public discovery fields" (keys == ["desktop", "expires", "public_id"])
        fieldsMatch publicInfo (json% {desktop: $(← qr.getObjVal? "desktop"), public_id: $(← qr.getObjVal? "public_id")})
        requireThat "discovery leaked private data" ([marker, admin, token, secret].all (!discover.body.contains ·))
        requireThat "invitation exposed relay administration" (!qr.compress.contains admin)
      let route := "/api/host/corpora/" ++ identity
      withHost deviceDir fun device => do
        let pairing ← device.api "POST" "/api/host/pair" qr 202
        let pending ← untilSome "host consent prompt" do
          let status ← authority.api "GET" "/api/remote"
          let pending ← IO.ofExcept <| status.getObjVal? "pending"
          return if pending == .null then none else some pending
        let code ← IO.ofExcept <| stringField pending "confirmation_code"
        IO.ofExcept do
          requireThat "confirmation code missing" (!code.isEmpty)
          fieldsMatch pairing (json% {code: $(code)})
        let registry := do IO.ofExcept <| (← device.api "GET" "/api/host").getObjVal? "corpora"
        let before ← registry
        let denied ← request device.url "GET" (route ++ "/api/state")
        IO.ofExcept <| verifyDenial "pending device" 404 denied before (← registry) [marker, admin, token, secret]
        let peer ← IO.ofExcept <| pending.getObjVal? "peer"
        discard <| authority.api "POST" "/api/remote/approve" (json% {peer: $(peer)})
        let pairId ← IO.ofExcept <| stringField pairing "id"
        discard <| untilSome "approved device registration" do
          let state ← device.api "GET" ("/api/host/pair/" ++ pairId)
          return if (← IO.ofExcept <| stringField state "state") == "approved" then some () else none
        let remoteState ← device.api "GET" (route ++ "/api/state")
        IO.ofExcept do
          discard <| uniqueRecord (← remoteState.getObjVal? "agents") "name" (toJson marker)
          fieldsMatch (← remoteState.getObjVal? "corpora") (json% {id: $(identity)})
        let before ← authority.api "GET" "/api/remote"
        let refused ← request device.url "POST" (route ++ "/api/remote/revoke") (json% {})
        let after ← authority.api "GET" "/api/remote"
        IO.ofExcept do
          fieldsMatch before (json% {paired: true, invitation: null})
          verifyDenial "remote cannot administer pairing" 403 refused (← before.getObjVal? "fingerprint")
            (← after.getObjVal? "fingerprint") [marker, admin, token, secret]
        let hidden ← request relay "GET" discoveryPath
        IO.ofExcept <| requireThat "paired device remained discoverable" (hidden.status == 404)
      withHost deviceDir fun device => do
        let saved ← device.api "GET" (route ++ "/api/corpora")
        IO.ofExcept <| fieldsMatch saved (json% {id: $(identity)})
        let revoked ← authority.api "POST" "/api/remote/revoke" (json% {})
        IO.ofExcept <| fieldsMatch revoked (json% {revoked: true, relay_deleted: true})
        let before ← authority.api "GET" "/api/corpora"
        let denied ← request relay "GET" ("/api/rooms/" ++ mailbox ++ "/messages") none #[s!"Authorization: Bearer {token}"]
        IO.ofExcept <| verifyDenial "revoked mailbox" 401 denied before (← authority.api "GET" "/api/corpora") [marker, admin, token, secret]
    withHost authorityDir fun authority => do
      IO.ofExcept <| fieldsMatch (← authority.api "GET" "/api/remote") (json% {paired: false, enabled: false, invitation: null})

end Spec.Founder
