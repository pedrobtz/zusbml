#!/bin/sh
# Extract the fixed BioModels (tools/biomodels.txt) from the BioModels
# Database release 31 archive into a directory that is never committed or
# shipped (design section 7). Tests find it through ZUSBML_BIOMODELS.
#
# The archive is pinned by checksum, so the models never change under the
# tests; the live download service answers CI runners with 403.
#
# Usage: tools/fetch-biomodels.sh [dest]   (default: biomodels)
set -eu

release=2017-06-26
archive=BioModels_Database-r31_pub-sbml_files.tar.bz2
sha256=b6fba03e9441157196cf566b493b64f3e2d08a07294786c672c9518a4c110532
url=https://ftp.ebi.ac.uk/pub/databases/biomodels/releases/$release/$archive
dest=${1:-biomodels}
list=$(dirname "$0")/biomodels.txt
ids=$(grep -v '^#' "$list")

missing=0
for id in $ids; do
  [ -s "$dest/$id.xml" ] || missing=1
done
if [ "$missing" = 0 ]; then
  echo "BioModels already in $dest"
  exit 0
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
curl -fsSL --retry 3 -o "$tmp/$archive" "$url"
echo "$sha256  $tmp/$archive" | shasum -a 256 -c - >/dev/null
members=""
for id in $ids; do
  members="$members BioModels_Database-r31_pub-sbml_files/curated/$id.xml"
done
# shellcheck disable=SC2086
tar -xjf "$tmp/$archive" -C "$tmp" $members
mkdir -p "$dest"
for id in $ids; do
  mv "$tmp/BioModels_Database-r31_pub-sbml_files/curated/$id.xml" "$dest/"
done
echo "$(ls "$dest"/*.xml | wc -l | tr -d ' ') BioModels in $dest"
