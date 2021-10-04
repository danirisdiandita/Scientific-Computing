


program tise

    implicit none
    real*8, dimension(0:100) :: x
    real*8, dimension(3,0:100) :: psi
    real*8 :: a, b, right, left, down, psi_end, knsquare, h
    real*8, parameter :: diff = 10e-6
    integer :: i, level
    open(10, file = "tise.txt")
    20 format(2e14.5)
    a = 0.0
    b = 1.0
    h = (b-a)/100.
    x(0:100) = [(i*h, i = 0,100)]
    psi(0) = 0.
    psi(1) = 0.1
    psi_end = 0.
    knsquare = 5.
    dkn = 1.
    do i = 1, 100
        left = 2*(1.-(5./12.)*h*h*knsquare)
        right = 1. + (1./12.)*h*h*knsquare
        down = 1. + (1./12.)*h*h*knsquare
        psi(level,i+1) = (left*psi(level,i)-right*psi(level,i-1))/down
    end do
    do i = 1, 100
        write(10,20) i*h, psi(i)
    end do

end program tise
