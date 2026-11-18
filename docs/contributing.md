# Contributing

- Keep responsibilities together: `corpora` (state/work/host), `app`
  (shell/features/theme), `remote` (relay/control/protocol), `environments` (native).
- Recursively split architectural responsibilities into three meaningful parts;
  domain needs take precedence. Do not create empty wrappers.
- State must not import Work or Host; Work must not import Host or native code.
- Keep dependency pins in `build/dependencies`, model prompts in `corpora/prompts`,
  build tools in `build`, and public-behavior tests in `spec`.
- Use lowercase directories and unique Haskell module names within each executable.
- Run the [tests](testing.md). Do not commit generated bundles or dependency caches.

Read the [license](../license/LICENSE.md) and retain third-party notices.
