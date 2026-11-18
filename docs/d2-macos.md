# macOS D2 native adapter

Base: `d47edde9a698f55f592de0dcf894e3f1942e4fd5`; branch `dev/d2-macos`.
All implementation and fixtures belong to the dedicated macOS worktree. No
Windows files or shared transaction policy were edited.

## Design and ownership

The shared `Update`, `Installation`, `UpdateState` and `UpdateBackup` modules
remain the only update transaction. macOS supplies process, filesystem, signature,
launch and preferences operations. There is no Sparkle integration, service,
second journal or second Coordinator.

The native entry point calls the shared installed-version authorization. The
backend does so too, using the two CPP guard extensions approved by the shared
owner. Each process owns one installation lease descriptor. Whole-file POSIX
record locks support shared running versions, an exclusive Coordinator and an
atomic downgrade; descriptors close on exec. The existing Corpora locks remain
unchanged. A candidate watches Coordinator pipe closure and checks the durable
selection before deciding whether it must exit. Its backend retains its own
lease until shutdown. The old UI confirms backend shutdown and sweeps its
existing owned session before handing off.

The stable Starter has its own immutable bundle and contains a signed seed
image. It bootstraps the per-user installation once and calls the shared Starter
selection. Every version is a complete sealed `.app`. No runtime state is written
inside a signed bundle. Publisher verification requires an Apple Developer ID
certificate from the installed team; the extracted app must also satisfy the
installed app's designated requirement and Gatekeeper assessment.

Native preferences use the existing Config-derived suite. Export/import covers
the complete suite, not cfprefsd's implementation files. Preference edits durably
refresh the mirror as well as clean exit/update, so a normal crash cannot cause
startup to import an older saved draft. Window geometry now
belongs to that suite. Config's plist therefore participates in the existing
snapshot digest and recovery. Third-party WebKit databases remain out of scope.

## Platform evidence

Local host: Apple Silicon, macOS 26.6 (25G72), APFS with ownership enabled.
GHC 9.14.1; minimum package target macOS 13.0. The native adapter was compiled with
Clang warnings enabled; package audits checked every Mach-O's architecture,
deployment target, internal dependencies and strict code signature after mounting
and copying the DMG.

Native probes established these details that affected the implementation:

- `acl_get_file(..., ACL_TYPE_EXTENDED)` returns `NULL`/`ENOENT` for an existing
  file with no extended ACL. That represents the empty ACL, not a missing file.
- `copyfile(COPYFILE_ALL)` preserved modification time but initialized a new
  file's creation time from it. Explicit `setattrlist(ATTR_CMN_CRTIME)` restored
  the original nanosecond creation timestamp and made fingerprints agree.
- Foundation normalized `/private/var/...` to `/var/...`. Comparing a normalized
  requested executable with a raw `NSRunningApplication.executableURL` missed
  the running selected app. Both sides are now resolved before comparison.

Native menu qualification also reproduced `SIGPIPE` when `hdiutil` inherited
stdout from an exited Coordinator. Staging now captures that output separately.
Cocoa's deferred termination cannot be entered from the same timer callback that
delivers its reply, so successful handoff queues termination after that callback
returns. A further metadata probe confirmed that copying directory metadata
removes target-only ACL/xattr additions.

The local `flock(2)` manual explicitly permits a gap during lock conversion.
`fcntl(2)` record-lock replacement avoids that gap; a native contender probe
confirmed the downgraded shared owner blocked an exclusive contender while a
second shared process entered. Local APFS accepted both directory `fsync` and
`F_FULLFSYNC`. Publication uses exclusive temporary creation, a full file flush,
rename, directory fsync and a full device flush. Staged bundles are recursively
fsynced and their containing directory fully flushed before returning to the
shared transaction. These are interruption tests,
not a physical power-loss qualification.

Apple documents the distinction between identity and a code hash in
[Code Signing Requirements](https://developer.apple.com/documentation/technotes/tn3127-inside-code-signing-requirements/),
the signing/notarization rules in
[Code Signing In Depth](https://developer.apple.com/library/archive/technotes/tn2206/),
and the full-flush operation in
[fcntl](https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/fcntl.2.html).
The release validation path enforces those requirements; local signing cannot
establish their release behavior.

## Verification

`spec/macos_update_spec.rb` contains two installed-app scenarios, driven through
native Accessibility and real Coordinator/Starter processes:

1. Initial Starter launch, signed A/B/C bundles, Workspace Ready/commit, native
   draft retention, Current/Previous rotation, stale recovery preserving new
   work (including a completed fixture-provider request after C commits), metadata
   checks and reopening the stable Starter.
2. Candidate suspension while Pending, Coordinator death, candidate lease
   release, preservation of the original backup, Starter recovery, xattr and
   draft restoration, and deletion of the failed target.

Run against the local production image:

```sh
SAPIENS_TEST_D2_MAC_DMG="$PWD/.build/d2-delivery/Sapiens.dmg" \
  ruby submodules/libs4/ruby/bundle.rb exec rspec spec/macos_update_spec.rb
```

For an explicitly isolated `d2-fixture` build, set `SAPIENS_TEST_D2_MAC_PAYLOAD`
to its payload DMG as well. The first scenario then presses the native **Update &
Restart** menu for both transitions, including quiesce, preference flush and
backend handoff. This uses the existing shared compile-time fixture path;
production builds ignore its environment variable. Never publish fixture builds.
All scenarios use temporary installations and separate provider/Data/Config
fixtures; no paid model or real user Data/Config is used.

## Release and integration limits

- No valid signing identity was available (`security find-identity` returned
  zero). All local images are ad-hoc signed. Developer ID, notarization,
  Gatekeeper acceptance of quarantined downloads and TCC grant continuity need
  signed release fixtures on supported Macs.
- LaunchServices registration, exact selected-version launch and native app
  identity are exercised locally. The Starter and running UI have different
  bundle IDs. Dock pinning, App Translocation and stable shortcuts through
  removal of the oldest bundle need dedicated release qualification. User Dock
  settings and TCC databases were not modified for these tests.
- The host is macOS 26.6. Mach-O deployment audits do not replace a run on macOS 13.
- Unsupported filesystems, links and immutable flags fail closed. This is not
  support for network shares, case-sensitive roots differing only by case,
  arbitrary filesystem writers or third-party WebKit recovery.
- Windows final `2ba7eb7196c77e0f3278d6c1737368e4d8cf2f71` removes Windows uninstall
  handling from shared Installation and moves it to the Windows Starter. The
  macOS implementation does not depend on that command. Integrate it before
  joint-platform regression; neither branch is merged to main here.

The implementation deliberately reused dependencies and process lifecycle code,
removed no behavioral guarantees, and added no locks beyond the existing
installation lease and Corpora locks. Investigation preceded automation; the
metadata and ownership findings were fixed before adding broader scenarios.

## Final targeted results

- Production installed E2Es: **2 examples, 0 failures**, seed 1664, 61.92 s
  (`final-production-e2e.log`).
- Native-menu fixture E2Es: **2 examples, 0 failures**, seed 45975, 87.4 s
  (`final-native-menu.log`). This includes drafts durable before clean quit.
- Clean source packaging: **1 example, 0 failures**, seed 26405, 129 s.
- Focus-sensitive browser rerun: **2 examples, 0 failures**, seed 21433, 25.96 s.
- Final production DMG checksums, mounted-copy strict signatures, dependency and
  deployment audits passed. Clang `-Wall -Wextra -Werror` checks passed for both
  native adapter files (with CommonCrypto deprecation warnings suppressed).

## Handoff record

Production diff against the frozen base: **622 added / 22 deleted / +600 net
lines**, including Cabal/build wiring. Shared transaction modules and Windows
files are unchanged. The only shared source entry-point change is the approved
macOS backend installation-lease guard in `corpora/Main.hs`.

The final local production artifacts are in `.build/d2-delivery`; the explicitly
non-distributable fixture artifacts are in `.build/d2-final-fixture`. Detailed
build, package, native probe and RSpec logs are under `.build/d2-evidence`.
These local artifacts are intentionally not committed.

The broader regression run was interrupted before a final summary. It had two
desktop input rejections caused by lost foreground ownership. Both affected
examples passed in the focused rerun (2 examples, 0 failures; seed 21433). Do not treat this branch as having a
completed green full-suite run. Cross-platform integration, the full suite and
release qualification remain required before merging/releasing.
