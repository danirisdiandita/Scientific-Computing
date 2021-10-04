program groundstate

    implicit none
    real*8, dimension(0:1000) :: y1, y0, s, ksquare1, ksquare2, ksquare
    real*8, dimension(3,0:1000) :: y
    real*8, parameter :: pi = 4.*atan(1.)
    real*8 :: a, b, h, tol, yend, dk
    integer :: n, i, level
!initial conditions
10 format(4e14.6)
20 format(2e14.6)
    a = 0.0
    b = 1.0
    n = 1000
    h = (b-a)/real(n)
    y1(0) = 0.0 
    y1(1) = 0.01
    y1(2:1000) = 10.0
    !ksquare = 4.*pi*pi
    ksquare = 1.0
    s = 0.0
    tol = 1.e-4
    yend = 0.0
    dk = 5.0
    call numerov(y1, ksquare, s, h)
    y0 = y1
    do while(abs(y1(1000) - yend) > tol)
         ksquare = ksquare + dk
         call numerov(y1, ksquare, s, h)
         if (y0(1000)*y1(1000) < 0) then
            ksquare = ksquare - dk
            dk = 0.5*dk
         end if
    end do
    
    do i = 0,1000
        write(*,20) i*h, y1(i)
    end do
end program groundstate

include "sub_numerov.f95"
