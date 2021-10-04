gfortran harmonicgitugitu.f95 -o harmonic.exe
./harmonic.exe
gnuplot
plot "harmonic.txt" u 1:2
