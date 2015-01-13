set term post eps
set output "2-curves.eps"
set title "Comparison between two reverse-count functions"
set xlabel "List length"
set ylabel "Time"
plot '2-curves.dat' using 1:2 with lines linecolor rgb "blue", \
     '2-curves.dat' using 1:3 with lines linecolor rgb "red"
