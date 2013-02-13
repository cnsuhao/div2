#!/bin/bash

# scan through the tv directory and find all newly downloaded files
#   used to show users what was downloaded within the last week
#   on a CGI page

cd /mnt/storage/videos/tv/
for i in {7..0}; do
	day=`date --date "$i days ago" +"%A - %b %d"`;

	if [ $i == 0 ]; then
		day=`echo $day | sed 's/.*day/Today/'`;
	fi;

	first=1	
	for f in `find . -ctime $i -name "*.avi" | sort -f`; do
		if [ $first == 1 ]; then
			echo $day
			echo "---------------------------"
			first=0
		fi;

		show=`echo $f | sed 's/^.*\/\(.*\).S[0-9][0-9].*/\1/' | tr "." " "`
		sn=`echo $f | sed 's/^.*.S\([0-9]*\)E[0-9]*.*/\1/'`
		ep=`echo $f | sed 's/^.*.S[0-9]*E\([0-9]*\).*/\1/'`
		pth=`echo $f | sed 's/^..//'`

		echo "$show - Ssn. $sn Ep. $ep ($pth)"
	done;
	if [ $first == 0 ]; then
		echo ""
		echo ""
	fi;
done;

