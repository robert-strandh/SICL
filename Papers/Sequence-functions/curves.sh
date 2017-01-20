#!/bin/sh

# if less than two arguments supplied, display usage 
if [ "$#" -lt 1 ]
then 
	echo "\nusage: $0 file"
	exit 1
fi

NAME=$1

GP=$NAME.gp

TITLE="Comparison between two find functions (SBCL/SICL)"
echo gp=$GP
OUTPUT=eps

SAVE=$IFS
IFS='-'
set
DATATYPE=$1
TEST=$2
TYPE=$3
KEY=$4
IFS=$SAVE

cat << _EOF_ > $GP
set term $OUTPUT dashed
set output "$NAME.$OUTPUT"
set title "$TITLE"
set key on inside center top
set xlabel "$DATATYPE length ($TEST $TYPE $KEY)"
set ylabel "Time in seconds"
_EOF_

STRING="'"$NAME"'"
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
