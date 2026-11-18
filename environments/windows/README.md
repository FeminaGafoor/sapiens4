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

An update reserves the Data Root under its existing writer locks before it
publishes Pending. Another installation may open that Data Root or a registered
child once the attempt has settled. Config can also be reused between
installations after the previous owner has closed and its decision is Settled.
Use a separate Config folder for simultaneously running installations. Keep
earlier builds that do not understand D2 ownership away from roots undergoing
an update. Recovery refuses if either physical root was replaced, even when the
folder name is unchanged.

Installation lease acquisition waits up to 15 seconds. The ready deadline is
one hour to allow migrations, followed by 60 seconds for activation. Closing
the candidate before commit, if its window responds, cancels the attempt.
After a precommit failure, recovery waits for the candidate's job to terminate.

### Persistent failure after commit

Use the installed shortcut once to retry the selected release. If it still
fails, close every Sapiens process using these roots. Before any repair, copy
the live Data and Config trees to fresh private directories, preserving
metadata, and save the installation's `update-state.json`. Keep that capture
and the retained Previous bundle and backup until the repair is verified.
Use a clean installation with separate Data and Config to test a repaired
release against disposable copies of the capture. Set `SAPIENS_DATA_DIR` and
`SAPIENS_CONFIG_DIR` to those copies before launching its shortcut. Remove the
copied `.sapiens-config-owner` only in the new Config copy so the clean
installation can claim it. Verify the work created after commit before moving
users to the repaired installation.

If a return to the earlier release is required, use the matching retained
release and a separate copy of the Previous backup alongside the saved live
capture. Leave the committed installation and its live roots intact. The
Previous backup predates work accepted by the committed release.

On Windows, copied file metadata is applied before flushing the file handle.
Directory metadata and whole-snapshot survival across physical power loss
still require qualification on the supported volume layouts.

The snapshot covers Corpora databases, app-owned files and attachment drafts,
remote pairing and provider configuration, and Config. Native client preferences
store workspace settings, unsent text drafts, the active Corpora and theme in
Config before the snapshot. Desktop remote accounts use the host registry and
bridge files under Data. The excluded WebView2 profiles contain reconstructible
shell caches and guest browsing state. Pairing in the separate browser relay
app uses that browser origin's IndexedDB. It is outside the desktop transaction.
