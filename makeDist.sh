#!/bin/sh
DIST_FILE=./.local/nostrrent-dist.zip
./mill jar
./mill zipRuntimeDeps
mv out/zipRuntimeDeps.dest/deps.zip $DIST_FILE
zip -j $DIST_FILE out/jar.dest/*.jar
echo "Distribution: $DIST_FILE"

