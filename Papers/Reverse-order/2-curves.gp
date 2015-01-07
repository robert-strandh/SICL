set term post eps
set output "coloring.eps"
set title "Coloring on rectangular grids 6 x N"
set xlabel "N"
set ylabel "Time"
plot 'coloring.dat' using 1:2 with lines linecolor rgb "red"

