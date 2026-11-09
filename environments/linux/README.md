# Ubuntu adapter

The Linux host can run without a display. The [native desktop](NATIVE.md) opens Sapiens in a GTK/WebKit window and uses Blindly4 for computer control. Desktop development targets Ubuntu 22.04 and 24.04 LTS on amd64 with GNOME, including GNOME sessions inside WSL2.

## Run the host

Use GHC 9.14.1, Cabal 3.18.2, Node 24 and the pinned submodules. Tests use Ruby 3.4 and Bundler. From the repository root, run

```sh
git submodule update --init --recursive
runghc build/Build.hs
./environments/linux/start.sh
```

The host prints its local workspace URL. The default mode is `native`. Computer commands use Blindly4 and require a suitable desktop session. The data directory defaults to `$XDG_DATA_HOME/sapiens4` when XDG_DATA_HOME is absolute, otherwise `$HOME/.local/share/sapiens4`. Use `SAPIENS_DATA_DIR` to override it or pass `--data-dir` on the command line.

## Run the desktop

Install the GTK, WebKitGTK and AT-SPI prerequisites in the [desktop build instructions](NATIVE.md#build-and-run), then run

```sh
runghc build/Build.hs --desktop
submodules/blindly4/linux/build.sh release
SAPIENS_LINUX_BACKEND=desktop ./environments/linux/start.sh
```

For a software-rendered X11 session, run `./environments/linux/virtual.sh COMMAND ARGUMENTS...`. The [desktop guide](NATIVE.md) covers the native Debian package and GNOME qualification. The [compatibility audit](AUDIT.md) records earlier test results and remaining hardware and enterprise coverage.
