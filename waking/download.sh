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
http://stout.hampshire.edu/~acg10/waking/Waking_Act_1.avi
http://stout.hampshire.edu/~acg10/waking/Waking_Act_2.avi
http://stout.hampshire.edu/~acg10/waking/Waking_Act_3.avi
http://stout.hampshire.edu/~acg10/waking/Waking_Act_4.avi
EOL` )
NURLS=${#URLS[@]}

NAMES=( $(echo ${URLS[@]/*\//}) )

MD5S=( "903baeaa3571dc9e9606afd7b6ad8f7c" "bbc77ccd544afd76605f95e4f67cebb0" "9cd4cd190e0988805e4d5cdfd8316b02" "351e0cc3da8ddc833015df17f0b30581" )

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
