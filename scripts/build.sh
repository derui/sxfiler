#!/bin/bash

BUILD_MODE=$1

mkdir -p /opt/libs
git clone https://github.com/derui/migemocaml /opt/libs/migemocaml
git clone https://github.com/derui/jsonrpc-ocaml /opt/libs/jsonrpc-ocaml
opam install -y /opt/libs/migemocaml /opt/libs/jsonrpc-ocaml
opam install -y --deps-only -t ./sxfiler.opam

if [[ $BUILD_MODE == "linux" ]]; then
    dune build --display=short --profile release
fi

if [[ $BUILD_MODE == "windows" ]]; then
    opam install -y -t --deps-only ./sxfiler-windows.opam
    dune build --display=short -x windows --profile windows
fi
