#!/usr/bin/env bash
# Baut alle Haskell-Plugins nach bin/plugins/<xx>/<name>-<version>-<os>-<arch>[.exe]
# (Namenskonvention des Resolvers, Go-Namen für OS und Architektur).
#   ./build.sh          bauen
#   ./build.sh --test   Tests aller Pakete (auch des SDK)
#   ./build.sh --run    bauen und den Host des Kerns mit allen Konfigurationen starten
set -euo pipefail
cd "$(dirname "$0")"

case "$(uname -s)" in
    Linux) os=linux ;;
    Darwin) os=darwin ;;
    MINGW* | MSYS* | CYGWIN*) os=windows ;;
    *) echo "unbekanntes Betriebssystem $(uname -s)" >&2; exit 1 ;;
esac
case "$(uname -m)" in
    x86_64 | amd64) arch=amd64 ;;
    arm64 | aarch64) arch=arm64 ;;
    *) echo "unbekannte Architektur $(uname -m)" >&2; exit 1 ;;
esac
ext=""; [ "$os" = windows ] && ext=".exe"

case "${1:-}" in
    --test) exec cabal test all ;;
    "" | --run) ;;
    *) echo "Aufruf: $0 [--test|--run]" >&2; exit 2 ;;
esac

cabal build all
for f in plugins/*/*.cabal; do
    name=$(awk '$1 == "name:" {print $2}' "$f")
    version=$(awk '$1 == "version:" {print $2}' "$f")
    out="bin/plugins/${name:0:2}/${name}-${version}-${os}-${arch}${ext}"
    mkdir -p "$(dirname "$out")"
    cp "$(cabal list-bin "exe:$name")" "$out"
    echo "gebaut: $out"
done

if [ "${1:-}" = --run ]; then
    cd ../coremesh
    exec "./bin/host$ext" -config configs,../coremesh-erp/configs,../coremesh-erph/configs
fi
