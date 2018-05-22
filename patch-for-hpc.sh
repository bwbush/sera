#!/usr/bin/env nix-shell
#!nix-shell -i bash -p patchelf
patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 dist/build/sera/sera
