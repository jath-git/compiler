#!/bin/bash

if [ -f 'compile_one.sh' ]; then
    chmod +x ./compile_one.sh
else
    echo 'ERROR: compile_one.sh does not exist'
    exit
fi

lineCount=0

while IFS= read -r line; do
    lineCount=$(($lineCount+1))
    inputs=( )
    len=0

    for word in $line
    do
        inputs+=($word)
        len=$(($len+1))
    done
    
    if [ $len -eq 5 ]; then
        bash ./compile_one.sh ${inputs[0]} ${inputs[1]} ${inputs[2]} ${inputs[3]} ${inputs[4]} ${lineCount}
    elif [ $len -gt 0 ]; then
        echo "ERROR at LINE $lineCount: Does not have 5 arguments"
    fi
done < $1