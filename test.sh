#!/bin/sh
set -eu
cd "$(dirname "$0")"
mkdir -p .build/test .build/bin
ghc -isrc -outputdir .build/test test/Main.hs -o .build/bin/tests
.build/bin/tests
ghc -isrc -outputdir .build app/Main.hs -o .build/bin/minighc
.build/bin/minighc
ghc -fno-code -isrc src/MiniGHC/Calculi/SystemF.hs src/MiniGHC/Calculi/STLC.hs src/MiniGHC/Operators/Quaternion.hs
