#!/usr/bin/env bash
set -euo pipefail

APP_NAME="stregsystemcobtui"
BIN_DIR="${HOME}/.local/bin"
TARGET="build/app"

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Missing dependency: $1" >&2
    exit 1
  fi
}

require_cmd make
require_cmd cobc
require_cmd curl
require_cmd jq

make

mkdir -p "$BIN_DIR"
cp -f "$TARGET" "$BIN_DIR/$APP_NAME"

cat <<EOF
Installed: $BIN_DIR/$APP_NAME
Make sure $BIN_DIR is on your PATH.
Example:
  export PATH=\"$BIN_DIR:\$PATH\"
EOF
