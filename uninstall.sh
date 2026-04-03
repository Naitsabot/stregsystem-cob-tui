#!/usr/bin/env bash
set -euo pipefail

APP_NAME="stregsystemcobtui"
BIN_DIR="${HOME}/.local/bin"
TARGET="$BIN_DIR/$APP_NAME"
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/stregsystem-tui"

PURGE=0
if [[ "${1:-}" == "--purge" ]]; then
  PURGE=1
fi

if [[ -f "$TARGET" ]]; then
  rm -f "$TARGET"
  echo "Removed: $TARGET"
else
  echo "Not installed: $TARGET"
fi

if [[ $PURGE -eq 1 ]]; then
  if [[ -d "$CONFIG_DIR" ]]; then
    rm -rf "$CONFIG_DIR"
    echo "Removed config: $CONFIG_DIR"
  else
    echo "No config to remove: $CONFIG_DIR"
  fi
fi
