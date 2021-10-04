program monte1d

integer :: seed
data seed/987654321/
a = ran(seed); b = ran(seed)
print*, a, b
end program monte1d
