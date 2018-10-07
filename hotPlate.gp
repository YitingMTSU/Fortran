set xrange [1:501]
set yrange [1:501]
#set zrange [0:100]
set xlabel 'Column'
set ylabel 'Row'
set xtics out
set ytics out
unset border # turns off the border (so that we can see the edges)
#unset colorbox # uncomment to remove the color palette on the right
#set datafile separator ','
set palette color
set pm3d map
set pm3d corners2color c1 # corners2color c1 tells gnuplot not to average the neighboring 
set terminal gif

set title (sprintf("%04d",i))
set output (sprintf("%s%04d%s",'hotPlate',i,'.gif'))
splot (sprintf("%s%04d%s",'hotPlate',i,'.csv')) notitle
i = i + 1
if (i < 65) reread