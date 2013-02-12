#!/bin/bash

CHECKSUM_PATH=$1
DOMAIN=$2

# ensure checksum exists
if ! [ -e $CHECKSUM ]; then
    echo "ERROR: checksum does not exist"
    exit 1
fi

function parse_vars {
    CHECKSUM=$1
    shift 1
    FILE="$@"
    URL="$DOMAIN/$FILE"
}


function validate {
    VALID=( $(md5sum $FILE 2> /dev/null) )
    [[ "$CHECKSUM" = "${VALID[0]}" ]];
    return $?
}


function download {
    echo "Downloading '$FILE'..."
    curl -C - -# -o $FILE $URL
}



cat $CHECKSUM_PATH | while read LINE; do
    parse_vars $LINE

    # skip if file has already downloaded correctly 
    validate
    if [ $? -eq 0 ]; then
        echo "Skipping $FILE"
        continue
    fi

    while [ 1 ]; do
        download 
        validate

        if [ $? -eq 0 ]; then
            break
        else
            echo "There was a problem with the download.  Retrying..."
        fi
    done
done
