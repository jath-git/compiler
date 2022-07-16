#!/bin/bash

if ! [ -f 'compiler.rkt' ]; then
    echo 'ERROR: compiler.rkt does not exist'
    exit
fi

if ! [ -f $1 ]; then
    echo "ERROR at LINE $6: $1 does not exist"
    exit
fi

if ! [ $2 = '.' ] && ! [ $2 = 'scan' ] && ! [ $2 = 'parse' ] && ! [ $2 = 'analyze' ] && ! [ $2 = 'assembly' ] && ! [ $2 = 'binary' ];
then
    echo "ERROR at LINE $6: $2 is not valid second argument"
    exit
fi

if [ $3 != '.' ] && [[ $3 =~ [^0-9] ]]; then
    echo "ERROR at LINE $6: $3 is not valid integer"
    exit
fi

if [ $4 != '.' ] && [[ $4 =~ [^0-9] ]]; then
    echo "ERROR at LINE $6: $4 is not valid integer"
    exit
fi

if [ $3 = '.' ] && [ $4 != '.' ]; then
    echo "ERROR at LINE $6: Must give first integer as well"
    exit
fi

if [ $5 = '.' ]; then
    if [ $2 = '.' ] && [ $3 = '.' ] && [ $4 = '.' ]; then
        racket compiler.rkt < $1
    elif [ $2 != '.' ] && [ $3 = '.' ] && [ $4 = '.' ]; then
        racket compiler.rkt < $1 $2
    elif [ $2 != '.' ] && [ $3 != '.' ] && [ $4 = '.' ]; then
        racket compiler.rkt < $1 $2 $3
    elif [ $2 != '.' ] && [ $3 != '.' ] && [ $4 != '.' ]; then
        racket compiler.rkt < $1 $2 $3 $4
    fi
else
    if [ $2 = '.' ] && [ $3 = '.' ] && [ $4 = '.' ]; then
        racket compiler.rkt < $1 > $5
    elif [ $2 != '.' ] && [ $3 = '.' ] && [ $4 = '.' ]; then
        racket compiler.rkt < $1 $2 > $5
    elif [ $2 != '.' ] && [ $3 != '.' ] && [ $4 = '.' ]; then
        racket compiler.rkt < $1 $2 $3 > $5
    elif [ $2 != '.' ] && [ $3 != '.' ] && [ $4 != '.' ]; then
        racket compiler.rkt < $1 $2 $3 $4 > $5
    fi
fi
