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
