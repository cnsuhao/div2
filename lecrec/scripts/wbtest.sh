####################################
#### Whiteboard Post Process sh ####
####################################
# args are location of whiteboard recording, durration of recording (for ffmpeg)
root=`readlink -f $1`
length=`cat $1/INFO | awk '/duration/ {print $2;}'`
ffmpeg=/usr/local/bin/ffmpeg

echo $root

echo "duration: $length"
echo "Running Whiteboard Post Process..."

cd $root/lboard/
for f in *.tiff
do
    OUTF=$(echo "$f" | sed s/\.tiff$/.ppm/g)
    echo "$f $OUTF"
done
