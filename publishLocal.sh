#!/usr/bin/env bash

sbt "project lib" "+ publishLocal"

sbt "project plugin" "^ publishLocal"

sbt "project api-core"   "+ publishLocal" \
    "project api-consul" "+ publishLocal" \
    "project api-play23" "+ publishLocal" \
    "project api-play24" "+ publishLocal" \
    "project api-play25" "+ publishLocal" \
    "project api-play26" "+ publishLocal"