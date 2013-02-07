#Script to run Post Process on Screen Images
echo "Running Screen Post Process"
cd $1/screen/

for f in *.png; do
    d=`echo $f | sed 's/-[0-9]*//'`
    mv $f $d
done;

cp /home/paol/proc/allPass allPass
./allPass cap_000001.png

mkdir -p ../forUpload/scr
mv slide*.png ../forUpload/scr
mv slideInfo.txt ../forUpload/scr/slides.txt
