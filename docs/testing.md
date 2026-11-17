# Testing

Build the host and the Blindly4 driver for your platform. Use an unlocked
desktop with accessibility permission, curl and the Lean toolchain pinned in
`spec/lean-toolchain`. From the repository root:

```sh
runghc build/Build.hs
cd spec
lake test
```

Use `lake test -- headless` for service and relay scenarios, or
`lake test -- desktop` for native interaction. The runner uses temporary
data and a local model fixture. On Linux build with
`runghc build/Build.hs --desktop` and use the [X11 runner](../environments/linux/README.md).

The suite has 19 headless jobs and two desktop jobs. Sapiens4 no longer uses
Ruby or RSpec to build or run its tests. The pinned submodules still
contain their own development test tooling; it is not part of this test runner
or the application payload.

Coverage is not equivalent to the retired suite. Installer upgrade and rollback,
native guest navigation isolation, legacy remote clients, altered or expired
encrypted deliveries, and performance under load remain gaps. The release
candidate's native onboarding, provider discovery, model-settings UI and full
multi-provider execution matrix also need Lean coverage. The release integration
checks model-settings persistence and validation, plus MCP work and lifecycle
behavior. Group scenarios are intentionally excluded.
