#!/usr/bin/env bash

sbt "project lib" "+ publish"

sbt "project plugin" "^ publish"

sbt "project api-core"   "+ publish" \
    "project api-consul" "+ publish" \
    "project api-play23" "+ publish" \
    "project api-play24" "+ publish" \
    "project api-play25" "+ publish" \
    "project api-play26" "+ publish"