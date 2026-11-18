# Tests

Use Lean from `spec/lean-toolchain`, Node.js 22+, curl, Chrome/Chromium/Edge,
and the platform build tools. `SAPIENS_TEST_BROWSER` selects the browser executable.
Tests use temporary data and a local model fixture.

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
