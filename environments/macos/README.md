# Sapiens4 for macOS

To open data from an earlier installation, select **Use another folder**, choose
the existing Data Root, then select **Continue**. In the folder picker, **⌘⇧G**
can open a hidden path such as a checkout's `.sapiens4`.

On macOS 13+ Apple Silicon, mount `Sapiens.dmg`, copy **Sapiens4.app** to
Applications (or your own Applications folder), and open it. No Sapiens build
tools are needed. The app opens without Codex; model execution requires your
installed Codex CLI and existing `codex login`. Nothing changes Codex credentials.
The launcher exposes standard Homebrew locations, inherited absolute PATH entries
and the standalone install at `~/.local/bin/codex`. For npm installations in a
custom prefix or shell-managed Node installation, provide that installation's
absolute executable with `SAPIENS_CODEX_BINARY` when launching the app; Finder
does not run shell startup files. If using npm's JavaScript launcher, its Node
executable must also be on the launch environment's PATH. For example:

```sh
SAPIENS_CODEX_BINARY=/absolute/path/to/codex /Applications/Sapiens4.app/Contents/MacOS/Sapiens4
```

Before starting the backend, a fresh installation opens Welcome + Storage.
Start fresh initializes the empty default directory. Use another folder opens a
complete existing Data Root in place or initializes an empty selected folder.
The selected directory is validated first and saved only after Workspace loads;
later launches validate and reuse it. Without a saved selection or explicit\n`SAPIENS_DATA_DIR` override, storage always requires confirmation. Missing, invalid or locked data returns to
the storage screen without creating replacement data.

Onboarding and Workspace share one resizable window.\nThe native window stays responsive; Quit can cancel backend startup.
Workspace loading failures return to the storage screen. View > Reload App\nreloads the page while the owned backend is alive. An exited backend returns\nto storage recovery; reopening a selected folder starts its replacement. Early startup errors appear in a native alert.
Guest document integration loads from the app's packaged `guest.js`; a missing
or empty asset reports a startup error before the backend starts.
Quit Sapiens4 before replacing or removing the app. Quit stops its owned backend;
closing the Workspace window keeps the app running. Closing the storage window
cancels startup and quits. Replacing, removing and reinstalling
the app retain `~/Library/Application Support/Sapiens4/{data,config}`. The backend
log is `desktop-server.log` in the configuration directory, including when\n`SAPIENS_CONFIG_DIR` is set. Older Development directories remain untouched.
Use `SAPIENS_DATA_DIR` and `SAPIENS_CONFIG_DIR` at launch for explicit isolation.
For example, run `SAPIENS_DATA_DIR=/path/to/data /Applications/Sapiens4.app/Contents/MacOS/Sapiens4`.

## Build and verify

Install Command Line Tools, GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, npm,
Git and stable Rust. Initialize pinned submodules recursively. From the root:

```sh
git submodule update --init --recursive
./environments/macos/build.sh 0.1.0 .build/release-local
(cd .build/release-local && shasum -a 256 -c SHA256SUMS)
```

The output directory must be new. It receives the Starter and update-payload
DMGs plus `SHA256SUMS`.
Builds reuse compiler caches and assemble fresh temporary payloads. Local builds
are ad-hoc signed. Each Mach-O must be ARM64, support macOS 13, and resolve all
non-system dependencies inside the app; recursive relocation rejects basename
collisions. The final mounted image is copied, audited and signature-verified.

For a public release, set `SAPIENS_MAC_SIGN_IDENTITY` to a Developer ID Application
identity and `SAPIENS_MAC_NOTARY_PROFILE` to a configured `notarytool` keychain
profile, then add `--release`:

```sh
./environments/macos/build.sh 0.1.0 .build/release-public --release
```

Public builds require both credentials, sign nested code with Hardened Runtime,
notarize and staple the final DMG, then verify the installed app copy with
Gatekeeper and signature checks. Credentials are not stored in outputs.
The installed D2 acceptance scenarios are in `spec/macos_update_spec.rb`.
Set `SAPIENS_TEST_D2_MAC_DMG` to the Starter image and use an unlocked desktop
with Accessibility permission.
Native browser tabs, file picking and accessibility behavior retain their existing interfaces.

## D2 updates

The installed **Sapiens4.app** is a short-lived Starter. It installs a sealed seed
on first launch, then selects a version using the shared `update-state.json`.
The mutable installation lives in
`~/Library/Application Support/Sapiens4/installation`: `versions/<UUID>/Sapiens4.app`,
`backups/<UUID>`, the installation lease, and the single Settled/Pending decision.
Data and Config remain separate. `SAPIENS_INSTALL_DIR` selects an isolated
installation for development and testing; use it together with explicit Data
and Config directories.

Use **Sapiens4 → Update & Restart** after accepted work finishes. The old app
pauses admission, exports its preferences and stops its backend before handing
off to the temporary Coordinator. The target opens with admission closed, runs
the existing preparation, and reports Ready only after native navigation and
Workspace Ready. New work starts after the durable selection. An interrupted
Pending attempt is recovered by the Starter using the retained original version
and its verified backup. Current and Previous are retained; the next successful
update removes the older pair.

The installation, Data and Config used for updates must be on APFS, with separate
roots and ordinary files/directories. Symlinks, hard-linked files, special files,
and immutable/append-only flags are rejected before snapshot or recovery. Copies
preserve contents, permissions, ownership, extended ACLs, xattrs, modification
and creation times. Sapiens-owned NSUserDefaults (including drafts and window
geometry) are mirrored as `Config/macos-preferences.plist`; recovery imports that
complete domain through the preferences API. Unrelated WebKit storage is not
part of this mirror.

Packaging emits `Sapiens.dmg`, `Sapiens-Payload-mac-arm64.dmg` and `SHA256SUMS`.
The first contains the stable Starter and seed; releases expose the second as
the updater asset. Automatic downloads require a Developer ID publisher matching
the installed app, the installed app's designated requirement, and Gatekeeper
acceptance. Local ad-hoc builds are development artifacts and do not qualify
release signing, notarization or TCC continuity.

Keep the stable installed Starter as the launch shortcut. The running UI has
bundle identifier `com.sapiens4.desktop`; the Starter uses
`com.sapiens4.starter`. Dock pinning across rotation and Developer ID/TCC behavior
still require release qualification. See [native D2 evidence and limits](../../docs/d2-macos.md).
