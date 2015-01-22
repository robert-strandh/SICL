#!/bin/sh

# if less than two arguments supplied, display usage 
if [ "$#" -le 1 ] || [ "$#" -ge 3 ]
then 
	echo "\nusage: $0 version_number1 version_number2"
	exit 1
fi 

V1=$1
V2=$1

if [ "$#" -eq 2 ]
then 
	V2=$2
fi 

NAME=v$V1-vs-v$V2

TITLE="Comparison between two reverse-count functions $NAME"
cat << _EOF_ > $NAME.gp
set term post eps
set output "$NAME.eps"
set title "$TITLE"
set xlabel "List length"
set ylabel "Time"
plot '$NAME' using 1:2 with lines linecolor rgb "blue", \\
     '$NAME' using 1:3 with lines linecolor rgb "red"
_EOF_
