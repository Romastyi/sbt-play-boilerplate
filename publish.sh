#!/usr/bin/env bash

sbt "project lib" "+ publish"

sbt "project plugin" "^ publish"

sbt "api-play23/publish" \
    "api-play24/publish" \
    "api-play25/publish"