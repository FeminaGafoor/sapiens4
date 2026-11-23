# Tests

Use Lean from `spec/lean-toolchain`, Node.js 22+, curl, Chrome/Chromium/Edge,
and the platform build tools. `SAPIENS_TEST_BROWSER` selects the browser executable.
Tests use temporary data and a local model fixture.

Test cases and assertions live in Lean, including browser crypto vectors and
tamper rejection. JavaScript only bridges browser operations and returns observations.
Desktop jobs initialize a temporary workspace before launch; they do not cover onboarding.
If a browser exits during fixture startup, set `SAPIENS_TEST_BROWSER` to a working
Chrome/Chromium/Edge executable (Chrome was used for Windows qualification).

```sh
runghc build/Build.hs
./submodules/blindly4/macos/build.sh
lake -d spec test
```

`lake -d spec test -- headless`: service/relay jobs.
`lake -d spec test -- desktop`: native UI jobs; requires an unlocked desktop and
Accessibility permission. Run desktop tests serially.
`SAPIENS_TEST_BLINDLY4` selects the driver; `SAPIENS_TEST_MACOS_APP` selects a signed app bundle.

Linux: build with `--desktop`, then `sh environments/linux/qualify.sh NEW_REPORT_DIRECTORY`.
Windows: build the host and driver using their platform guides.

Coverage excludes installer lifecycle, native guest isolation, remote-client
UI, altered/expired encrypted delivery, load, native onboarding, provider
discovery, model-settings UI, full provider combinations, and Group scenarios.

Storage uses fresh LMDB fixtures through Append4; legacy-file migration tests are replaced by
crash/restart, ownership and public audit checks. See `docs/storage.md` and
`submodules/append4/README.md` for the storage contract and low-level checks.

Additional storage checks (pass the executable path returned by cabal list-bin):

```sh
cabal --project-dir=build --project-file=cabal.project build exe:append4-fixture exe:sapiens-storage-fixture
lean --run submodules/append4/spec/Main.lean PATH_TO_APPEND4_FIXTURE
lean --run spec/storage/Main.lean PATH_TO_SAPIENS_STORAGE_FIXTURE
```

These include relay expiry backlogs, room isolation, trust/ack index cleanup,
no transient payload history, map growth and commits without a flush clock.
On Windows/macOS, the integration fixture also verifies an actual update backup
with LMDB data and excluded runtime lock files.
