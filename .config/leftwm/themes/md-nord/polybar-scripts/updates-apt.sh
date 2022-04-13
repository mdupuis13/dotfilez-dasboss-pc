#!/usr/bin/env bash

#set -xe

message=""

fetchUpdates () {
  sudo apt-get update 1> /dev/null
  return $?
}

getNumberOfNormalUpdates () {
  updates=$(apt-get upgrade -s |grep -P '^\d+ upgraded'|cut -d" " -f1);

#  if [ "$updates" -gt 0 ]; then
      message="$updates"
#  fi

  return 0
}

#getNumberOfSecurityUpdates () {
  #updates=0
  #updates=$(apt-get upgrade -s |grep -P '^\d+ \w\ssecurity'|cut -d" " -f1);

  #if [ ! -z "$updates" ] && [ "$updates" -gt 0 ]; then
      #message+=" $updates sec"
  #fi

  #return 0
#}

printMessage() {
  echo $message
  return 0
}

fetchUpdates

if [ $? -eq 0 ]; then
  getNumberOfNormalUpdates
#  getNumberOfSecurityUpdates
fi


printMessage
