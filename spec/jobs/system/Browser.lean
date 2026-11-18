import io.CryptoWindow

open Lean
namespace Spec.SystemJobs

def preserveBrowserApplicationBehavior : IO Unit := IO.FS.withTempDir fun directory =>
  withHost directory fun host => withCryptoSession host fun browser => do
    let observed ← browser (json% {operation: "application"})
    IO.ofExcept do
      fieldsMatch (← observed.getObjVal? "malformedCrypto") (json% {unboxNull: "", randomFraction: "Invalid random length", missingPlaintext: "Invalid encoded value"})
      let #[paragraph] ← observed.getObjValAs? (Array Json) "markup" | throw "Expected one markup paragraph"
      fieldsMatch paragraph (json% {tag: "p", attrs: {id: "one"}, text: "hello café"})
      let #[text, strong, ignored] ← paragraph.getObjValAs? (Array Json) "children" | throw "Expected text, strong and comment nodes"
      fieldsMatch text (json% {text: "hello "})
      fieldsMatch strong (json% {tag: "strong", attrs: {}, text: "café", children: [{text: "café"}], source: "<strong>café</strong>"})
      requireThat "Comment node was not ignored" (ignored == json% {})
      fieldsMatch (← observed.getObjVal? "install") (json% {prevented: true, prompted: 1})
      fieldsMatch (← observed.getObjVal? "updating") (json% {failure: "Update unavailable", added: 2, removed: 2})
      fieldsMatch (← observed.getObjVal? "decodedQR") (json% {data: "sapiens-purescript", width: 320, height: 320})
      fieldsMatch (← observed.getObjVal? "caching") (json% {cachedStatus: 200, cachedSVG: true, fallbackStatus: 200})
      fieldsMatch (← observed.getObjVal? "storage") (json% {
        saved: {text: "café 🧬"}, all: [{text: "café 🧬"}], removed: null, invalidStorage: true})
      fieldsMatch (← observed.getObjVal? "preferences") (json% {
        theme: "dark", active: "owner", oldWorkspace: null, oldSnapshot: null,
        unrelated: "keep", forbidden: null, restoredOnce: true, rejectedOrigin: true,
        onlyAllowed: true, publishedLight: true})
      fieldsMatch (← observed.getObjVal? "audio") (json% {
        pieces: 2, firstFinal: false, lastFinal: true, firstIndex: 0, lastIndex: 1,
        firstStart: 0, lastStart: 1000, duration: 1000, segments: 2, rate: 16000,
        dataLength: 32000, firstSample: 32767, secondSample: 16383,
        tailLength: 2, tailSample: 0, sameStop: true, stopped: 1, closed: 1,
        clipping: [-32767, -32767, 0, 32767, 32767]})
      fieldsMatch (← observed.getObjVal? "setup") (json% {
        initialBlocked: true, consentEnables: true, pickingBlocks: true,
        directoryLabel: "…/Ada/Sapiens", submitted: 1,
        selected: {storage: "existing", directory: "C:\\Users\\Ada\\Sapiens", terms_accepted: true, terms_version: "terms-v1"},
        recovered: true, consentBlocks: true, visible: true,
        legal: {storage: "document", document: "privacy-policy", terms_accepted: false, terms_version: "terms-v1"}})

end Spec.SystemJobs
