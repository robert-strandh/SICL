#!/bin/bash

function capitalize {
    echo $1 | perl -ne 'print ucfirst lc'
}

function lower {
    echo $1 | perl -ne 'print lc'
}

function upper {
    echo $1 | perl -ne 'print uc'
}

function test_string {
    if [ $1 == EGAL ]
    then
	echo '='
    else
	echo $1
    fi
}

# if less than two arguments supplied, display usage 
if [ "$#" -lt 1 ]
then 
	echo "\nusage: $0 file"
	exit 1
fi

FILENAME=$1

NAME="${FILENAME%.*}"
echo NAME=$NAME

GP=$NAME.gp

TITLE="Comparison between two find functions (SBCL/SICL)"
echo gp=$GP
OUTPUT=eps

SAVE=$IFS
IFS='-'
set $NAME
SEQTYPE=$(capitalize $1)
#TEST=$(test_string $(lower $2))
TEST=$(test_string $(lower $2))
THIRD=$(lower $3)
IFS=$SAVE

echo TEST=$TEST SEQTYPE=$SEQTYPE $THIRD

cat << _EOF_ > $GP
set term $OUTPUT dashed
set output "$NAME.$OUTPUT"
set title "$TITLE"
set key on inside center top
set xlabel "$SEQTYPE length ($TEST $THIRD)"
set ylabel "Time in seconds"
_EOF_

STRING="'"$FILENAME"'"
echo STRING=$STRING


echo plot '\' >> $GP

echo "    " $STRING using 1:2 title \'SBCL\' with lines ',\' >> $GP
echo "    " $STRING using 1:3 title \'SICL\' with lines ',\' >> $GP

# N=2
# for i in $FIELDS ;
# do 
# 	echo FIELD=$i N=$N
# 	echo "    " $STRING using 1:$N title \'$i\' with lines ',\' >> $GP
# 	N=$(expr $N + 1)
# done
