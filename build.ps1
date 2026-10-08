# Windows: baut alle Haskell-Plugins nach bin/plugins/<xx>/<name>-<version>-windows-<arch>.exe
#   .\build.ps1          bauen
#   .\build.ps1 -Test    Tests aller Pakete (auch des SDK)
#   .\build.ps1 -Run     bauen und den Host des Kerns mit allen Konfigurationen starten
param([switch]$Test, [switch]$Run)
$ErrorActionPreference = 'Stop'
Set-Location $PSScriptRoot

if ($Test) { cabal test all; exit $LASTEXITCODE }

$arch = if ($env:PROCESSOR_ARCHITECTURE -eq 'ARM64') { 'arm64' } else { 'amd64' }
cabal build all; if ($LASTEXITCODE) { exit $LASTEXITCODE }
foreach ($f in Get-ChildItem plugins/*/*.cabal) {
    $text = Get-Content $f
    $name = ($text | Select-String '^name:\s*(\S+)').Matches[0].Groups[1].Value
    $version = ($text | Select-String '^version:\s*(\S+)').Matches[0].Groups[1].Value
    $out = "bin/plugins/$($name.Substring(0,2))/$name-$version-windows-$arch.exe"
    New-Item -ItemType Directory -Force -Path (Split-Path $out) | Out-Null
    $exe = cabal list-bin "exe:$name"; if ($LASTEXITCODE) { exit $LASTEXITCODE }
    Copy-Item $exe $out -Force
    Write-Host "gebaut: $out"
}
if ($Run) {
    Set-Location ../coremesh
    & './bin/host.exe' -config 'configs,../coremesh-erp/configs,../coremesh-erph/configs'
}
