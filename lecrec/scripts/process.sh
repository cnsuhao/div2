#!/bin/bash

lck="/var/lock/manic.lck";

if [ -f $lck ]; then
    echo "Locked"
    exit 0
fi

if [ "$1" == "" ]; then
    next=( $(find /overflow -iname "20*" -type d) )
    next=${next[0]}
else
    next=$1;
fi

if [ "$next" == "" ];then
    echo "Nothing to do"
    exit 0
fi

touch $lck


echo "running processes"
cd $next/screen
/home/paol/proc/spp.sh $next &> $next/proj.log &
vgaPID=$!

cd $next
/home/paol/proc/wbpp.sh $next &> $next/wb.log &
wbPID=$!

echo "waiting for background processing to complete"
wait $vgaPID
wait $wbPID

echo "copying logs"
cp $next/*.log $next/forUpload
cp $next/INFO $next/forUpload

echo "moving to processed"
done=`echo $next | sed 's/overflow-processed/'`
mkdir -p `dirname $done`
mv $next $done

echo "ready for upload"
/home/paol/proc/upload.sh $done

if [ $? -eq 0 ]; then
    upload=`echo $done | sed 's/overflow-uploaded/'`
    mkdir -p `dirname $upload`
    mv $done $upload
    echo "uploaded"
else
    echo "there was an error with the upload"
fi

rm -rf $lck
