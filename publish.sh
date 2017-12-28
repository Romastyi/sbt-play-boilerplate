#!/usr/bin/env bash

sbt "project lib" "+ publish" \
    "project plugin" "^ publish" \
    "api-play23/publish" \
    "api-play24/publish" \
    "api-play25/publish"