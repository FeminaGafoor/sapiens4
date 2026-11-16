# Code guidelines

## rTernarity: structure by responsibility

Developers and coding agents must organize each architectural split into **three
meaningful child responsibilities**, applying the same rule recursively when a
child needs subdivision. Name parts for their purpose; keep related code together.

Functional requirements take precedence: use more branches when the domain needs
them, and briefly explain that choice in the change description. A cohesive
collection may contain any number of file leaves (for example, prompts or tests).
Entry points, package markers, tool-required paths, and external dependencies keep
their required shape. Files are real leaves; do not create empty folders, wrappers,
or speculative code to simulate infinite recursion or satisfy a count.

Refactors must preserve behavior and avoid import cycles. Verify meaningful
boundaries with focused tests; check the three-child invariant only for structures
explicitly modeled as ternary, not every filesystem directory. Expand recursive
visualizations only to the depth needed for the current view.

Application responsibilities are explicit at the root: `corpora/` for agents,
`app/` for presentation, `remote/` for remote access and the relay server, and
`environments/` for native integration. These four product areas are intentional;
do not hide the relay or platform code just to make the root contain three folders.

The corpora behavior has three groups: `state/` (domain values, persistence,
population, Notes and workspace records), `work/` (model/computer execution,
delegation, groups and schedules), and `host/` (service lifecycle, management,
HTTP and assets). `prompts/` is its model-prompt resource collection. State must
not import Work or Host; Work consumes State and must not import Host or native
implementations. Host composes both. Shared primitives remain independent of
consumers. Persistent `agentpy/` data paths are unchanged by source organization.

Remote access divides into `relay/` (server, mailboxes and provisioning),
`control/` (host bridge, client and registration), and `protocol/` (cryptography,
transport and receipts). Control may call the corpora service; the service does
not depend on remote transports. The local HTTP entrypoint is `HostServer.hs`;
the relay entrypoint is `remote/relay/Server.hs`.

App responsibilities are `shell/`, `features/`, and `theme/`; the shell divides
model, interaction and rendering, with entrypoints/assets as leaves. Native code,
resources and packaging stay together under `environments/macos/` and
`environments/windows/`. Add `environments/linux/` when implementation exists.
Shared build orchestration, UI compilation, launchers and the relay Dockerfile
live in `build/`. Tests under `spec/` separate jobs, pure checks and IO.

Owned directories use lowercase names. Cabal declares each Haskell responsibility
folder as a source root, so modules use their capitalized filenames without
requiring uppercase directory wrappers. Keep module names unique within each
executable; native executables have their own platform-specific source roots.

Prompts, tests, docs and licenses are cohesive collections. External dependencies
under `submodules/libs4/` and `submodules/blindly4/` keep their required layouts;
their internal directory names and tool-owned caches are not application structure.

Review changes against these boundaries; tests exercise public behavior rather
than asserting the names, shape, or import graph of private implementation code.
Persistent `agentpy/` data paths remain unchanged by source reorganization.

## Third-party dependencies

Keep dependency manifests, lockfiles, vendored libraries, package caches and
installed test dependencies in the `libs4` submodule. Sapiens4 owns application
and test source, with explicit imports into libs4. Do not add a root Gemfile,
package manifest, requirements file, or checked-in third-party build output.
Update dependencies in libs4 first, then advance its submodule pin here.

## Hardcoded model prompts

All hardcoded model prompts must live in the repository's `corpora/prompts/`
directory as separate, descriptively named `.md` files, such as
`corpora/prompts/conversation.md` and `corpora/prompts/computer-use.md`.

Application code must load these files instead of embedding prompt text in source
code. This includes system instructions, prompt prefixes/suffixes, reference
wrappers. Dynamic values may be inserted using
template placeholders; user messages, Notes contents, and generated state are data,
not hardcoded prompts.

## Observable behavior

Run the [Lean 4 tests](testing.md) against public entrypoints, with temporary
data, a controlled model executable and Blindly4 for desktop interactions.

## Contribution licensing

Read [LICENSE.md](../license/LICENSE.md) and expressly accept the
[Contributor License Agreement](../license/CONTRIBUTOR_LICENSE_AGREEMENT.md) before your
contribution is merged. Identify the pull request or commit range and the
agreement revision in your acceptance. Disclose third-party material and obtain
any required employer or other rights-holder authorization.

Maintainers must verify and retain the acceptance record for every contributing
rights holder before merging. This is a manual merge requirement; no automated
CLA service is configured. Earlier contributions need separate clearance.
