#!/bin/bash

# Create a thumbnail montage for a video

d=`dirname $1`
vid=`basename $1`
base=`echo $vid | sed "s/\.[^.]*$//"`

cd $d

dur=`ffmpeg -i $vid 2>&1 | awk '/Duration/ { print $2 }' | sed "s/,//" | awk -F":" ' { print ($1 * 3600) + ($2 * 60) + $3}' | xargs printf "%d" 2> /dev/null`
step=`expr $dur / 30`

for i in {0..29}; do 
    let sec=step*i
    let sec+=1
    ffmpeg -ss $sec -y -i $vid -f mjpeg -vframes 1 -s 240x176 -an /tmp/$base`printf "%03d" $i`.jpg &> /dev/null
done

montage /tmp/$base*.jpg -tile x6x5 -geometry 240x176+0+0 /tmp/$base.jpg
convert /tmp/$base.jpg -gravity North -background White -splice 0x16 -annotate +0+2 "$base.jpg" $base.jpg

rm /tmp/$base*.jpg 


