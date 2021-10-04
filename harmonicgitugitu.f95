program energylevel

    implicit none
    real*8, dimension(0:1000) ::x, y1, y0, s, ksquare1, ksquare2, ksquare, yy
    real*8, dimension(7,0:1000) :: y
    real*8, parameter :: pi = 4.*atan(1.)
    real*8 :: a, b, h, tol, yend, dE, E
    integer :: n, i, level
    real*8, external :: k2
!initial conditions
10 format(8e14.6)
20 format(2e14.6)
    open(30, file = "harmonic.txt")
    a = -5.
    b = 5.
    n = 1000
    h = (b-a)/real(n)
    y1(0) = 0.0 
    y1(1) = 0.01
    y1(2:1000) = 10.0
    E = 0.1
    !ksquare = 4.*pi*pi
    ksquare = [(k2(E, a+i*h), i = 0,1000)]
    s = 0.0
    tol = 1.e-4
    yend = 0.0
    dE = 0.5
    
    call numerov(y1, ksquare, s, h)
    y0 = y1
    do level = 1, 7
        do !while (abs(y1(1000) - yend) > tol)
             E = E + dE
             ksquare = [(k2(E, a+i*h), i = 0,1000)]
             call numerov(y1, ksquare, s, h)
             if (y0(1000)*y1(1000) < 0) then
                E = E - dE
                dE = 0.5*dE
                ksquare = [(k2(E, a+i*h), i = 0,1000)]
             end if
             if (abs(y1(1000) - yend) < tol) exit
        end do
        print*, E
        y(level, 0:1000) = y1
        y0 = y1
        y1(1000) = y1(1000) + 2.*tol
        dE = 0.5
    end do
    
    do level = 1,7
        yy(0:1000) = [(y(level, i), i = 0,1000)]
        call normalize (yy,h)
        y(level, 0:1000) = [(yy(i), i = 0,1000)]
    end do
    
    x(0:1000) = [(i*h, i = -500,500)]
    do i = 0,1000
        !write(30,10) x(i), (y(level,i), level = 1, 5)
        write(30,10) x(i), y(6,i), y(7,i)
    end do
end program energylevel

include "sub_numerov.f95"

subroutine normalize(y,h)
    
    implicit none
    real*8, dimension (0:1000), intent(inout) :: y
    real*8, intent(in) :: h
    real*8 :: total
    integer :: i
    total = 0.
    do i = 0, 1000
        total = total + y(i)*y(i)
    end do
    y = y/((total *h)**0.5)
    
end subroutine normalize

real*8 function k2(E, x)
    real*8, intent(in) :: E, x
    k2 = 2.*(E-0.5*x*x)
end function k2




