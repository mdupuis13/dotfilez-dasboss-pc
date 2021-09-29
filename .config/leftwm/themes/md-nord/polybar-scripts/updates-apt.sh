#!/bin/sh

apt-get update 1> /dev/null
updates=$(apt list --upgradable 2> /dev/null | grep -c upgradable);

if [ "$updates" -gt 0 ]; then
    echo "$updates pkg"
else
    echo ""
fi
