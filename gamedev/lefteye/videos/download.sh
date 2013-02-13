#!/bin/bash

# quit if no curl
if [ -z "$(which curl)" ]
then
    echo "Error: this program requires curl"
    exit 1
fi

# variables
DIR=`dirname $0`
URLS=( `cat <<EOL
http://stout.hampshire.edu/~acg10/lefteye/01_main_menu.avi
http://stout.hampshire.edu/~acg10/lefteye/02_spike_death.avi
http://stout.hampshire.edu/~acg10/lefteye/03_ceiling_spike.avi
http://stout.hampshire.edu/~acg10/lefteye/04_watery_grave.avi
http://stout.hampshire.edu/~acg10/lefteye/05_rafters.avi
http://stout.hampshire.edu/~acg10/lefteye/06_various_camera_modes_on_hills.avi
http://stout.hampshire.edu/~acg10/lefteye/07_overhead_fall.avi
http://stout.hampshire.edu/~acg10/lefteye/08_over_the_shoulder.avi
http://stout.hampshire.edu/~acg10/lefteye/09_splining_camera.avi
http://stout.hampshire.edu/~acg10/lefteye/10_trigger_boxes.avi
http://stout.hampshire.edu/~acg10/lefteye/11_trigger_settings.avi
EOL` )
NURLS=${#URLS[@]}

NAMES=( $(echo ${URLS[@]/*\//}) )


MD5S=( "07cd2642ef1ffce8697366cbce089298" "c4387373327fee6a0b88213b5033276c" "9d5469fcf95d9bf21c9d66e16d970f52" "ae7e7fc8596a58ea113bdeb14363af8b" "520ad61de39e40d8f49bad747b706108" "892da92958a52edd5128fa77d0f5509e" "840eda1e4c2bc0472ab1d4899d96dca4" "ddf418f68de607cf20a0c8f016252de4" "11306cde5ba1acb603398636bfb59655" "866cdf6e567ab6301890d4c02b16eb5b" "fe393a0aaaca2504716998b101b2f604" )

# I hate doing this long hand but the loops weren't working
declare -a FILESIZES
for URL in "${URLS[@]}"; do
    FILESIZES+=(`curl -sI $URL | grep "Content-Length" | sed "s/[^0-9]//g"`)
done


load_vars() {
    N=$1
    N=$((N+1))
    NAME=${NAMES[$1]}
    FILE=$DIR/$NAME
    URL=${URLS[$1]}
    MD5=${MD5S[$1]}
    LOCAL_SIZE=$(stat -c%s $FILE 2> /dev/null)
    REMOTE_SIZE=${FILESIZES[$1]}

    if [ -z "$LOCAL_SIZE" ]; then
        LOCAL_SIZE=0
    fi
}

checksum() {
    echo "$MD5  $NAME" | md5sum -c - &> /dev/null
    return $?
}

download() {
    checksum 
    ret=$?
    if [ $ret == 0 ]; then
        echo "$NAME downloaded" 
    else
        if [ "$LOCAL_SIZE" -lt "$REMOTE_SIZE" ]; then
            FLAGS="-C -"
        fi

        echo "Downloading $NAME [$N/$NURLS] ... "
        curl $FLAGS -# -o $FILE $URL
        echo ""
    fi
}

quit () {
    tput sgr0
    for p in "${PIDS[@]}"; do
        ps $p | grep "curl" > /dev/null
        if [ $? == 0 ]; then
            kill $p
        fi
    done
    exit
}

# basic downloader
basic_downloader() {
    for ((i=0;i<$NURLS;i++)); do
        load_vars $i
        download
    done
    echo "Enjoy!"
}


# advanced downloader
#   just for giggles
#   requires tput, md5sum, and du
#   to simultaneously download all 4
pretty_downloader() {

    # spool up background download
    declare -a PIDS
    for ((i=0;i<$NURLS;i++)); do
        load_vars $i
        download &> /dev/null &
        sleep 0.1 
        pid=$!
        PIDS+=( $(ps -ef | awk -v p=$pid '$3==p { print $2 }') )
    done

    trap 'quit' SIGINT
    trap 'quit' EXIT

    COLS_0=0
    LNS_0=0
    while [ True ]; do 

        # check if redraw is needed
        c=`tput cols`
        l=`tput lines`
        if [ $c -ne $COLS_0 ] || [ $l -ne $LNS_0 ]; then
            COLS_0=$c
            LNS_0=$l

            tput clear
            tput bold
            for ((i=0;i<NURLS;i++)); do
                load_vars $i
                printf $NAME
                tput cuf 7
                printf "%%  ["
                tput el
                tput cup $i $((c - 2))
                printf "]"
                echo ""
            done
        fi


        for ((i=0;i<NURLS;i++)); do
            load_vars $i

            tput bold
            tput setaf 7

            PERCENT=$(echo "$LOCAL_SIZE / $REMOTE_SIZE * 100" | bc -l)

            tput cup $i $(( ${#NAME} + 2 ))
            printf "%5.1f" $PERCENT

            if [ $LOCAL_SIZE -lt $REMOTE_SIZE ]
            then
                tput setaf 3
            else
                tput setaf 2
            fi
 
            COLS=$(( $(tput cols) - ${#NAME} - 13 ))
            STEPS=$(echo "$COLS * $PERCENT / 100" | bc)

            tput cuf 4
            printf "%${STEPS}s" " " | sed "s/./#/g"

            
        done
        echo ""       

        # allow quitting
        tput sgr0

        # quit when no more background processes
        jobs > /dev/null
        if [ -z "$(jobs)" ]; then
            break;
        fi

        sleep 0.5;
    done

}

if [ -z "$(which tput)" ]; then
    echo "You need tput for the 'pretty' downloader"
    basic_downloader
else
    pretty_downloader
fi

exit 0
