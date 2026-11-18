# Sapiens4 for Windows

Run **Sapiens-Setup-win-x64.exe** on Windows 10/11 x64. Install WebView2 if prompted, then close and relaunch Sapiens4.
For model execution, install Codex CLI and run `codex login`.

Data and config default to `%LOCALAPPDATA%\Sapiens4\data` and its sibling `config`.
The first launch requires storage confirmation. Previously confirmed storage
reopens automatically. Choose **Use another folder** to open existing data outside the
installation. Quit before uninstalling. If an update was interrupted, open Sapiens
from its shortcut to finish recovery first. Uninstall removes marked version directories
and updater backups from the installation and leaves external Data and Config folders.

## Build

Use GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, npm, Git, a POSIX shell, GNU Make,
Rust with MSVC, objdump and Inno Setup 6+. Choose a new output directory.

```powershell
git submodule update --init --recursive
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0 -OutputDirectory .build\release-x64
```

For signed releases, install Windows SDK signtool, set `SAPIENS_WINDOWS_CERT_THUMBPRINT`
and add `-PublicRelease`. Native ARM64 packaging requires a matching toolchain and still awaits device qualification.

Tracked `AppIcon.ico` and `AppIconDark.ico` are the packaging inputs.
Packaging verifies their hashes and both SVG source hashes against `icons.sha256`.
After changing a favicon SVG, regenerate both variants and commit the updated resources.

```powershell
.\environments\windows\regenerate-icons.ps1
```

### Update payload packaging

The build emits `Sapiens-Setup-<runtime>.exe` for ordinary installation and
`Sapiens-Payload-<runtime>.exe` for extraction into an empty version directory.
Both contain the same complete application. A public release signs and verifies
both containers and the native payload files; local development builds remain
explicitly unsigned fixtures.

The payload container is compiled with `StagePayload`. It requires an explicit
`/DIR=<new-version-directory>`, refuses nonempty destinations (including owned
installations), and creates no shortcut, uninstall record, uninstaller or
post-install launch. Failed extraction directories must be abandoned; retry into
a fresh empty directory. This artifact only stages bytes: it neither selects a
version nor authorizes data migration. The updater must verify the container's
Authenticode publisher before executing it and commit version selection through
its transaction protocol. **Update & Restart** checks GitHub Releases for the
product repository and stages only a newer numeric release. The signed payload
includes its release version. Both the container and its staged executable must
pass Authenticode verification and match the installed publisher's subject and
public key. A publisher key change requires a separately verified installer.

Before Ready, the target opens and migrates local data, prepares registered Corpora,
and verifies their snapshots while application writes and remote work are gated.
The Coordinator publishes the new selection and releases job ownership before
signaling commit. External workers start after commit. If startup then fails,
the committed version remains selected for the next launch. Older artifacts are
pruned only after the target acknowledges Active.

Recovery takes its managed roots from the verified backup and acquires the
existing Corpora locks for surviving directories. Missing child directories
are reconstructed from the backup, which remains available until the original
selection has been republished.

The snapshot covers Corpora databases, app-owned files and attachment drafts,
remote pairing and provider configuration, and Config. Native client preferences
store workspace settings, unsent text drafts, the active Corpora and theme in
Config before the snapshot. Desktop remote accounts use the host registry and
bridge files under Data. The excluded WebView2 profiles contain reconstructible
shell caches and guest browsing state. Pairing in the separate browser relay
app uses that browser origin's IndexedDB. It is outside the desktop transaction.
