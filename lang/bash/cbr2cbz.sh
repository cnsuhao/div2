#!/bin/bash
f=$(readlink -f "$1")                # find file, not symlink
d=$(mktemp -d)                       # create temp folder
                                     
cd $d                                # go to new folder
echo -n "Converting $1..."                                                        
rar e "$f" &> /dev/null              # extract cbr silently                    
zip "${f%%.cbr}.cbz" * &> /dev/null  # zip silently                                        
echo "OK"                                                        
cd - > /dev/null                     # go to prev directory             
rm -rf $d                            # remove temp               
