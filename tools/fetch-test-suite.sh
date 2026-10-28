#!/bin/sh
# Download the SBML Test Suite's semantic cases into a directory that is
# never committed or shipped (design D5): the suite is LGPL 2.1 and 37 MB
# unpacked. Tests find it through ZUSBML_TEST_SUITE.
#
# Usage: tools/fetch-test-suite.sh [dest]   (default: sbml-test-suite)
set -eu

version=3.5.0
zip=semantic_tests_v$version.zip
sha256=523bed258b384f761da211c233c5b4a7f28cd6cc2197ff709c6e253a6eacea77
url=https://github.com/sbmlteam/sbml-test-suite/releases/download/$version/$zip
dest=${1:-sbml-test-suite}

if [ -f "$dest/VERSION" ] && [ "$(cat "$dest/VERSION")" = "$version" ]; then
  echo "SBML Test Suite $version already in $dest"
  exit 0
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
curl -fsSL -o "$tmp/$zip" "$url"
echo "$sha256  $tmp/$zip" | shasum -a 256 -c - >/dev/null
unzip -q "$tmp/$zip" -d "$tmp/unpacked"

rm -rf "$dest"
mkdir -p "$dest"
mv "$tmp/unpacked/semantic/"* "$dest/"
echo "$version" > "$dest/VERSION"
echo "SBML Test Suite $version in $dest"
