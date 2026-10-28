#!/bin/sh
# Download the fixed BioModels (tools/biomodels.txt) into a directory that
# is never committed or shipped (design section 7). Tests find it through
# ZUSBML_BIOMODELS.
#
# Usage: tools/fetch-biomodels.sh [dest]   (default: biomodels)
set -eu

dest=${1:-biomodels}
list=$(dirname "$0")/biomodels.txt
mkdir -p "$dest"

for id in $(grep -v '^#' "$list"); do
  file="$dest/$id.xml"
  [ -s "$file" ] && continue
  # biomodels.org is where www.ebi.ac.uk/biomodels redirects downloads; an
  # anonymous client gets 403 from CI runners there.
  curl -fsSL --retry 3 -A "zusbml (https://github.com/pedrobtz/zusbml)" \
    -o "$file.tmp" \
    "https://www.biomodels.org/model/download/$id?filename=${id}_url.xml"
  mv "$file.tmp" "$file"
done
echo "$(ls "$dest"/*.xml | wc -l | tr -d ' ') BioModels in $dest"
