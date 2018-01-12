#!/usr/bin/env bash

sbt "project lib" "+ publishSigned"

sbt "project plugin" "^ publishSigned"

sbt "project api-play23" "+ publishSigned" \
    "project api-play24" "+ publishSigned" \
    "project api-play25" "+ publishSigned" \
    "project api-play26" "+ publishSigned"