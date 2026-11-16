import Lake
open Lake DSL

package «sapiens-behavior»

lean_lib Contracts where
  globs := #[.submodules `jobs, .submodules `io, .submodules `pure]

lean_exe fixtureProvider where
  root := `io.Provider

@[test_driver, default_target]
lean_exe behavior where
  root := `Main
  needs := #[`@/fixtureProvider]
