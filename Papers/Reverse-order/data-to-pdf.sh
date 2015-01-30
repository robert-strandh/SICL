#!/bin/sh

# if less than two arguments supplied, display usage 
if [ "$#" -lt 1 ]
then 
	echo "\nusage: $0 data-file"
	exit 1
fi
NAME=$1
sh curves.sh  $NAME
gnuplot $NAME.gp
ps2pdf $NAME.eps
