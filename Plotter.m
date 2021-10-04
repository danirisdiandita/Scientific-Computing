fid = fopen('harmonic.txt');
s = textscan(fid,'%f %f %f', 'headerlines',23);
fclose(fid);
x = s{1};
y = s{2};
z = s{3};

plot(x,y,x,z,'--');
legend('n = 0','n = 1');
title('Wavefunctions of Schrodinger Equations');
grid;