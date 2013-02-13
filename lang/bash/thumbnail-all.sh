#!/bin/bash

# create a thumbnail for any video which doesn't already have one

thumb=/usr/local/bin/thumbnailer.sh
IFS=$'\n'

if [ -z "$1" ]; then
	echo "missing directory"
	exit 1;
fi

fc=`find $1 -type f -exec mimetype \{} \; | grep "video"`

find $1 -type f | while read file; do
	mime=`mimetype $file | awk '{print $2}' | sed "s/\/.*//g"`
	image=${file%.*}.jpg
	if [ "$mime" = "video" ] && [ ! -e "$image" ]; then
		echo -n "$file ... "
		$thumb $file
		echo "ok"
	fi
done
