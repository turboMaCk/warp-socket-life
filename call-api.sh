#!/usr/bin/env bash

# Print commands to console
set -X

API=http://localhost:3000

echo "So what is wrong huh? Lets try to close connection from client"

curl -v $API &
sleep 1
echo "Kill it"
kill %1

echo "Hmm this reminds me of a https://www.tenable.com/audits/items/CIS_Apache_HTTP_Server_2.4_v2.2.0_L1.audit:6dc2e6074c2ecfe00969b2b85f8b2f0a"
echo "lets try few more"

# for ((i = 0 ; i < 10000 ; i++)); do
#     curl -v $API &
#     kill %1
# done

# echo "done with 10k requests on my side!"
