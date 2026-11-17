# Linux

Ubuntu 22.04/24.04 amd64; desktop control requires X11.
Use the [source prerequisites](../../README.md), then:

```sh
sudo apt-get install -y build-essential pkg-config libgtk-3-dev libwebkit2gtk-4.1-dev libjson-glib-dev libatspi2.0-dev libx11-dev at-spi2-core dbus-user-session dbus-x11 libglib2.0-bin
runghc build/Build.hs --desktop
submodules/blindly4/linux/build.sh release
SAPIENS_LINUX_BACKEND=desktop ./environments/linux/start.sh
```

Install and sign in to a model CLI separately. `SAPIENS_DATA_DIR` selects storage.
Build without `--desktop` for a headless host.
For Debian packaging: `sh environments/linux/package.sh .build/sapiens4.deb`.

Tests require `xvfb xauth openbox x11-utils libgtk-4-dev`:
`sh environments/linux/qualify.sh NEW_REPORT_DIRECTORY`.
[Driver limits](../../submodules/blindly4/linux/README.md) · [Remote control](../../docs/remote.md)
