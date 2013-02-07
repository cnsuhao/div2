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
    convert $f $OUTF
done
/home/paol/proc/process frame000001-*.ppm

echo "creating dup frames"
fps=`/home/paol/proc/framDup.py`

cd frames/
mkdir -p $root/forUpload/presenterVID
echo "making movies with $fps fps"

#$ffmpeg -r $fps -i frame_%8d.ppm -an -pass 1 -vcodec libx264 -vpre slow_firstpass -b 200k -threads 8 -f rawvideo -y /dev/null
#$ffmpeg -r $fps -i frame_%8d.ppm -an -pass 2 -vcodec libx264 -vpre slow -b 200k -threads 8 -y $root/forUpload/presenterVID/video.mp4

$ffmpeg -r $fps -i frame_%8d.ppm -an -vcodec libtheora -y $root/forUpload/presenterVID/video.ogv
echo "movie done"

echo "muxing audio"
mkdir -p $root/forUpload/presenter
#$ffmpeg -i $root/audio.wav -i $root/forUpload/presenterVID/video.mp4 -vcodec copy -acodec libfaac -ab 128k -threads 8 -y $root/forUpload/presenter/video.mp4
$ffmpeg -i $root/audio.wav -i $root/forUpload/presenterVID/video.ogv -vcodec copy -acodec libvorbis -ab 128k -threads 8 -y $root/forUpload/presenter/video.ogv
echo "muxing finished"

cd $root
echo "converting audio"
mkdir -p $root/forUpload/presenterMIC
$ffmpeg -i audio.wav -threads 8 -ab 128k forUpload/presenterMIC/audio.mp3
echo "audio done"


echo "moving whiteboard"
cd $root/LeftCam/
mkdir -p $root/forUpload/wbl
for i in rtBoard-*; do 
    convert $i $root/forUpload/wbl/`basename $i .ppm`.png;
done
echo "correcting times"
sed -i 's/.ppm/.png/g' pass1TimeL.txt
sed -i 's/pass1/rtBoard/g' pass1TimeL.txt
mv pass1TimeL.txt $root/forUpload/wbl/slides.txt
echo "whiteboard done"

