import io.Host

open Lean
namespace Spec

def verifyCrypto (call : Json → IO Json) : IO Unit := do
  let invoke := fun (method : String) (args : Array Json) => call (json% {operation: "crypto", method: $(method), args: $(args)})
  let value := fun (method : String) (args : Array Json) => do
    let result ← invoke method args
    IO.ofExcept <| fieldsMatch result (json% {ok: true})
    return result.getObjValD "value"
  let check := fun (label : String) (actual expected : Json) => IO.ofExcept <| requireThat label (actual == expected)
  let rejects := fun (label method : String) (args : Array Json) => do
    let result ← invoke method args
    IO.ofExcept <| requireThat label (result.getObjValD "ok" == toJson false)
  let hex := fun (text : String) => value "hex" #[toJson text]
  let alice ← hex "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
  let bob ← hex "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
  let nonce ← hex "69696ee955b62b73cd62bda875fc73d68219e0036b7a0b37"
  let plaintext ← hex "be075fc53c81f2d5cf141316ebeb0c7b5228c52a4c62cbd44b66849b64244ffce5ecbaaf33bd751a1ac728d45e6c61296cdc3c01233561f41db66cce314adb310e3be8250c46f06dceea3a7fa1348057e2f6556ad6b1318a024a838f21af1fde048977eb48f59ffd4924ca1c60902e52f0a089bc76897040e082f937763848645e0705"
  let ciphertext ← hex "f3ffc7703f9400e52a7dfb4b3d3305d98e993b9f48681273c29650ba32fc76ce48332ea7164d96a4476fb8c531a1186ac0dfc17c98dce87b4da7f011ec48c97271d2c20f9b928fe2270d6fb863d51738b48eeee314a7cc8ab932164548e526ae90224368517acfeabd6bb3732bc0e9da99832b61ca01b6de56244a9e88d5f9b37973f622a43d14a6599b1f654cb45a74e355a5"
  let a ← value "publicKey" #[alice]
  let b ← value "publicKey" #[bob]
  check "Alice key vector" a (← hex "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")
  check "Bob key vector" b (← hex "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")
  check "NaCl ciphertext vector" (← value "perform" #[toJson "open", json% {secret: $(bob), peer: $(a), frame: {sender: $(a), nonce: $(nonce), ciphertext: $(ciphertext)}}]) plaintext
  check "NaCl empty-message vector" (← value "box" #[toJson "", nonce, b, alice]) (← hex "2539121d8e234e652d651fa4c8cff880")
  for message in ["", "Remote control ✓ 👩🏽‍💻", String.ofList (List.replicate 16384 'x')] do
    let plain ← value "encode" #[toJson message]
    let frame ← value "perform" #[toJson "seal", json% {secret: $(alice), peer: $(b), plaintext: $(plain)}]
    check "Host round trip" (← value "perform" #[toJson "open", json% {secret: $(bob), peer: $(a), frame: $(frame)}]) plain
    let nonce := frame.getObjValD "nonce"
    let cipher := frame.getObjValD "ciphertext"
    check "Remote client opens host message" (← value "unbox" #[cipher, nonce, a, bob]) (toJson message)
    check "Host and client use identical encryption" (← value "box" #[toJson message, nonce, b, alice]) cipher
    let size ← IO.ofExcept <| fromJson? (← value "byteLength" #[cipher]) (α := Nat)
    for position in [0, 15, size - 1] do
      let altered := frame.setObjVal! "ciphertext" (← value "flipByte" #[cipher, toJson position])
      rejects "Tampered frame accepted" "perform" #[toJson "open", json% {secret: $(bob), peer: $(a), frame: $(altered)}]
    let alterations := [frame.setObjVal! "sender" b,
      frame.setObjVal! "nonce" (← value "randomBytes" #[toJson (24 : Nat)]),
      frame.setObjVal! "ciphertext" (← value "truncate" #[cipher, toJson (15 : Nat)])]
    for altered in alterations do
      rejects "Invalid frame accepted" "perform" #[toJson "open", json% {secret: $(bob), peer: $(a), frame: $(altered)}]
    rejects "Wrong secret key accepted" "perform" #[toJson "open", json% {secret: $(← value "randomBytes" #[toJson (32 : Nat)]), peer: $(a), frame: $(frame)}]
  rejects "Low-order peer accepted" "box" #[toJson "work", nonce, (← value "zeros" #[toJson (32 : Nat)]), alice]
  rejects "Wrong nonce length accepted" "box" #[toJson "work", (← value "randomBytes" #[toJson (23 : Nat)]), b, alice]
  rejects "Malformed encoding accepted" "perform" #[toJson "seal", json% {secret: $(alice), peer: $(b), plaintext: "%%%"}]
  rejects "Unknown operation accepted" "perform" #[toJson "unknown", json% {}]
  let keys ← value "keyPair" #[]
  check "Generated public key mismatch" (← value "publicKey" #[keys.getObjValD "secretKey"]) (keys.getObjValD "publicKey")
  let first ← value "randomBytes" #[toJson (32 : Nat)]
  let second ← value "randomBytes" #[toJson (32 : Nat)]
  IO.ofExcept <| requireThat "Random tokens repeated" (first != second)

end Spec
