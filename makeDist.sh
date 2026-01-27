#!/bin/sh
set -eu
DIST_FILE=./.local/nostrrent-dist.zip
./mill clean
./mill jar
./mill zipRuntimeDeps
cp out/zipRuntimeDeps.dest/deps.zip $DIST_FILE
zip -j -r -0 $DIST_FILE out/jar.dest/*.jar
echo "Distribution: $DIST_FILE"
