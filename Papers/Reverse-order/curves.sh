#!/bin/sh

# if less than two arguments supplied, display usage 
if [ "$#" -lt 1 ]
then 
	echo "\nusage: $0 file"
	exit 1
fi

NAME=$1

GP=$NAME.gp

TITLE="Comparison between reverse-count functions"
echo gp=$GP

cat << _EOF_ > $GP
set term post eps
set output "$NAME.eps"
set title "$TITLE"
set xlabel "List length"
set ylabel "Time"
_EOF_

STRING="'"$NAME"'"
echo STRING=$STRING

FIELDS=$(IFS='-'; echo $NAME)


echo plot '\' >> $GP

N=2
for i in $FIELDS ;
do 
	echo FIELD=$i N=$N
	echo "    " $STRING using 1:$N title \'$i\' with lines ',\' >> $GP
	N=$(expr $N + 1)
done



