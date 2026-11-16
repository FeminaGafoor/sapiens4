import Lake
open Lake DSL

package «sapiens-behavior»

lean_lib Contracts where
  globs := #[.submodules `jobs, .submodules `support, .submodules `pure]

@[test_driver, default_target]
lean_exe behavior where
  root := `Main
