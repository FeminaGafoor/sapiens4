import io.Host

open Lean System
namespace Spec

abbrev CryptoWindow := Json → IO Json

def withCryptoSession (host : Host) (use : CryptoWindow → IO α) (verify := false) : IO α := do
  let root ← repoRoot
  let child ← IO.Process.spawn {
    cmd := "node", args := #["spec/io/BrowserCrypto.cjs", host.url] ++ (if verify then #["--verify"] else #[])
    cwd := root, stdin := .piped, stdout := .piped }
  try
    let ready ← IO.asTask child.stdout.getLine .dedicated
    let line ← untilSome "browser crypto window" (do
      if ← IO.hasFinished ready then return some (← IO.ofExcept ready.get)
      return none) 30000
    IO.ofExcept <| requireThat "browser crypto fixture failed" (line.trimAscii == "ready")
    use fun input => do
      child.stdin.putStrLn input.compress
      child.stdin.flush
      let reply ← IO.asTask child.stdout.getLine .dedicated
      let line ← untilSome "browser crypto result" (do
        if ← IO.hasFinished reply then return some (← IO.ofExcept reply.get)
        return none) 30000
      IO.ofExcept <| Json.parse line
  finally
    if (← child.tryWait).isNone then
      child.stdin.putStrLn ""
      child.stdin.flush
      try
        let status ← untilSome "browser shutdown" child.tryWait
        IO.ofExcept <| requireThat "browser shutdown failed" (status == 0)
      catch error =>
        try child.kill catch _ => pure ()
        discard child.wait
        throw error

def withCryptoWindow (host : Host) (use : IO α) (verify := false) : IO α :=
  withCryptoSession host (fun _ => use) verify

def withCryptoHost (directory : FilePath) (use : Host → IO α) : IO α :=
  withHost directory fun host => withCryptoWindow host (use host)

end Spec
