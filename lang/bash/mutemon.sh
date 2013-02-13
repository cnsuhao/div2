#!/bin/bash

# A simple script to automatically toggle headphone mute when plugged in
#   for some reason, wasn't working by default on new linux install


DEBUG=false

log () {
    if [ $DEBUG ]; then
       echo $1
    fi 
}

detect () {
    grep "on" "/proc/acpi/ibm/volume"
}


pstatus="previous status"
while [ 1 ]; do
    status=$(detect)
    if [ "$status" != "$pstatus" ]; then
        if [ -n "$status" ]; then
            log "Muted"
            amixer sset Master mute 1> /dev/null
        else
            log "Unmuted"
            amixer sset Master unmute 1> /dev/null
        fi
    fi
done
