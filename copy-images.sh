#!/bin/bash

SOURCE_DIR="$PWD/frontend/exe/images"
DEST_DIR="$PWD/dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/frontend-0.1.0.0/c/frontend/build/frontend/frontend.jsexe/images"


DEST_DIR_TEST="$PWD/dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/common-0.1.0.0/c/test/build/test/test.jsexe"

eval "rsync -r $SOURCE_DIR/ $DEST_DIR"
cp "$PWD/frontend/exe/index.html" "$DEST_DIR"
cp "$PWD/common/index.html" "$DEST_DIR_TEST"
