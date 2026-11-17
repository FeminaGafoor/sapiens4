# Contributing

- Keep responsibilities together: `corpora` (state/work/host), `app`
  (shell/features/theme), `remote` (relay/control/protocol), `environments` (native).
- Split architectural responsibilities into three meaningful parts when useful;
  domain needs take precedence. Do not create empty wrappers.
- State must not import Work or Host; Work must not import Host or native code.
- Keep dependency pins in `submodules/libs4`, model prompts in `corpora/prompts`,
  build tools in `build`, and public-behavior tests in `spec`.
- Use lowercase directories and unique Haskell module names within each executable.
- Run the [tests](testing.md). Do not commit generated bundles or dependency caches.

Read the [license](../license/LICENSE.md). Before merge, each contributing rights
holder must expressly accept the [CLA](../license/CONTRIBUTOR_LICENSE_AGREEMENT.md),
identifying the contribution and agreement revision. Disclose third-party material
and obtain required rights-holder authorization. Maintainers retain and verify acceptance.
