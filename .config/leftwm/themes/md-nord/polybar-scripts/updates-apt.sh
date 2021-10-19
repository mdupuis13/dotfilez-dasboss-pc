#!/bin/sh

message=""
sudo apt-get update 1> /dev/null
updates=$(apt-get upgrade -s |grep -P '^\d+ upgraded'|cut -d" " -f1);

if [ "$updates" -gt 0 ]; then
    message="$updates pkg"
fi

updates=0
updates=$(apt-get upgrade -s |grep -P '^\d+ \w\ssecurity'|cut -d" " -f1);

if [ ! -z "$updates" ] && [ "$updates" -gt 0 ]; then
    message+=" $updates sec"
fi

echo $message
