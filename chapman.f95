program chapman
    implicit none
    real :: a = -12.3, b = .123, c = 123.456
    write(*,200) a, b, c
    write(*,210) a, b, c
    200 format(' ', 2f6.3, f8.3)
    210 format(' ', 3f10.2)
end program chapman
