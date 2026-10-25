#!/bin/sh
# Verify the F* modules in dependency order, one file per invocation
# with a shared cache: this F* version rejects multi-file invocations,
# but each later module loads the earlier modules' .checked files from
# the cache dir. The cache is rebuilt from scratch on every run, so a
# stale artifact can never mask a failure.
# FSTAR_BIN may point at another fstar.exe; the default is the release
# unpacked under ~/workspace/.tools/fstar.
set -e
cd "$(dirname "$0")"
FSTAR_BIN="${FSTAR_BIN:-$HOME/workspace/.tools/fstar/fstar/bin/fstar.exe}"
rm -rf .fstar-cache
for m in ToolRegistry SecurityContracts ExecutionEngine Dispatcher; do
  "$FSTAR_BIN" --cache_checked_modules --cache_dir .fstar-cache "$m.fst"
done
