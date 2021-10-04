
program tise
    
    implicit none
    real*8, parameter :: pi = 4.*atan(1.)
    real*8 :: knsquare, left, right, down, h
    real*8, dimension(0:1000) :: y
    integer :: i
20 format(2e14.5)
    open(10, file = "numerov.txt")
    knsquare = 4*pi*pi
    y(0) = 1
    h = (1.-0.)/1000.
!    y(1) = 0.000001
    y(1) = y(0) + 0.5*h*h*(-knsquare*y(0))
    do i = 1, 1000
        left = 2*(1.-(5./12.)*h*h*knsquare)
        right = 1. + (1./12.)*h*h*knsquare
        down = 1. + (1./12.)*h*h*knsquare
        y(i+1) = (left*y(i)-right*y(i-1))/down
    end do
    do i = 0,1000
        write(10,20) i*h, y(i)
    end do
end program tise
