$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
& npm install --package-lock=false --include=dev --prefix (Join-Path $repo 'build/dependencies')
if ($LASTEXITCODE -ne 0) { throw 'Icon dependency installation failed' }
$paths = @()
foreach ($icon in @(@('favicon.svg','AppIcon.ico'),@('favicon-dark.svg','AppIconDark.ico'))) {
    $source = 'app/shell/' + $icon[0]
    $target = 'environments/windows/' + $icon[1]
    & node (Join-Path $repo 'environments/windows/svg-to-ico.cjs') (Join-Path $repo $source) (Join-Path $repo $target)
    if ($LASTEXITCODE -ne 0) { throw 'Icon generation failed' }
    $paths += @($source, $target)
}
$hashes = foreach ($path in $paths) {
    (Get-FileHash -Algorithm SHA256 -LiteralPath (Join-Path $repo $path)).Hash.ToLowerInvariant() + '  ' + $path
}
[IO.File]::WriteAllLines((Join-Path $PSScriptRoot 'icons.sha256'), [string[]]$hashes)
