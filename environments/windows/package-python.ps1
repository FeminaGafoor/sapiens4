# Assemble the pinned embedded Python runtime for a Sapiens4 Windows release.
param(
    [Parameter(Mandatory=$true)][ValidateSet('win-x64','win-arm64')][string]$Runtime,
    [Parameter(Mandatory=$true)][string]$TargetDirectory,
    [string]$PythonArchive,
    [string]$BuildPython = 'python'
)
$ErrorActionPreference = 'Stop'
$arch = if ($Runtime -eq 'win-arm64') { 'arm64' } else { 'amd64' }
$dependencies = Join-Path $PSScriptRoot '../../submodules/libs4/python'
$pin = Get-Content -Raw (Join-Path $dependencies 'runtime.json') | ConvertFrom-Json
$version = $pin.version
$abi = $pin.abi
$hash = $pin.sha256.$arch
if (-not $PythonArchive) {
    $cache = Join-Path $dependencies '.cache'
    New-Item -ItemType Directory -Force $cache | Out-Null
    $PythonArchive = Join-Path $cache "python-$version-embed-$arch.zip"
    if (-not (Test-Path $PythonArchive)) { Invoke-WebRequest "https://www.python.org/ftp/python/$version/python-$version-embed-$arch.zip" -OutFile $PythonArchive }
}
if ((Get-FileHash -LiteralPath $PythonArchive -Algorithm SHA256).Hash -ne $hash) { throw 'Embedded Python checksum mismatch' }
Expand-Archive -LiteralPath $PythonArchive -DestinationPath $TargetDirectory -Force
[IO.File]::WriteAllText((Join-Path $TargetDirectory "python$abi._pth"), "python$abi.zip`n.`n..`nimport site`n", [Text.UTF8Encoding]::new($false))
$wheelPlatform = if ($Runtime -eq 'win-arm64') { 'win_arm64' } else { 'win_amd64' }
$packages = Join-Path $TargetDirectory 'Lib/site-packages'
if (Test-Path -LiteralPath $packages) { Remove-Item -LiteralPath $packages -Recurse -Force }
& $BuildPython -m pip install --disable-pip-version-check --only-binary=:all: --platform $wheelPlatform --python-version $version --implementation cp --abi "cp$abi" --upgrade --target $packages -r (Join-Path $dependencies 'remote-client-requirements.txt')
if ($LASTEXITCODE -ne 0) { throw 'Packaged Python dependencies failed' }
