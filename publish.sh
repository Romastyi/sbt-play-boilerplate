#!/usr/bin/env bash

PUBLISH_COMMAND="publish"

case $1 in
    "" )
    ;;
    --local )
        PUBLISH_COMMAND="publishLocal"
    ;;
    --signed )
        PUBLISH_COMMAND="publishSigned"
    ;;
    * )
        echo "Invalid option: $1"
        exit 1
    ;;
esac

echo "Artifacts will published with '$PUBLISH_COMMAND' command."

sbt "project lib" "+ $PUBLISH_COMMAND"

sbt "project plugin" "^ $PUBLISH_COMMAND"

sbt "project api-client-core"   "+ $PUBLISH_COMMAND" \
    "project api-client-consul" "+ $PUBLISH_COMMAND" \
    "project api-client-play23" "+ $PUBLISH_COMMAND" \
    "project api-client-play24" "+ $PUBLISH_COMMAND" \
    "project api-client-play25" "+ $PUBLISH_COMMAND" \
    "project api-client-play26" "+ $PUBLISH_COMMAND"

sbt "project api-server-play23" "+ $PUBLISH_COMMAND" \
    "project api-server-play24" "+ $PUBLISH_COMMAND" \
    "project api-server-play25" "+ $PUBLISH_COMMAND" \
    "project api-server-play26" "+ $PUBLISH_COMMAND"