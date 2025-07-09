#!/usr/bin/env bash

# Print commands to console
set -X

API=http://localhost:3000

echo "So what is wrong huh? Lets try to close connection from client"

curl -v $API &
sleep 1
echo "Kill it"
kill %1
