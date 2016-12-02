set term eps dashed
set output "v0-v1-v7.eps"
set title "Comparison between reverse-count functions"
set key on inside center top
set xlabel "List length"
set ylabel "Time in seconds"
plot \
     'v0-v1-v7' using 1:2 title 'v0' with lines ,\
     'v0-v1-v7' using 1:3 title 'v1' with lines ,\
     'v0-v1-v7' using 1:4 title 'v7' with lines ,\
