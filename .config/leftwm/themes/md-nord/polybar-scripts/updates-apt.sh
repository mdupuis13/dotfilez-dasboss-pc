#!/bin/sh

updates=$(apt list --upgradable 2> /dev/null | grep -c upgradable);

echo " $updates pkg "

#if [ "$updates" -gt 0 ]; then
    #echo "$updates pkg"
#else
    #echo ""
#fi
