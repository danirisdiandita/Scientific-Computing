program trialsubnumerov

    implicit none
    real*8, dimension(0:1000) :: y1, y2, s, ksquare1, ksquare2, ksquare
    real*8, dimension(3,0:1000) :: y
    real*8, parameter :: pi = 4.*atan(1.)
    real*8 :: a, b, h, tol, yend
    integer :: n, i, level
!initial conditions
10 format(4e14.6)
20 format(3e14.6)
    a = 0.0
    b = 1.0
    n = 1000
    h = (b-a)/real(n)
    y1(0) = 0.0 ; y2(0) = 0.0
    y1(1) = 0.01 ; y2(1) = 0.01
    y1(2:100) = 10.0 ; y2(2:100) = 10.0
    !ksquare = 4.*pi*pi
    ksquare1 = 9. ; ksquare2 = 10.0
    s = 0.0
    tol = 1.e-6
    yend = 0.0
    call numerov(y1, ksquare1, s, h)
    call numerov(y2, ksquare2, s, h)
    do i = 0,1000
        write(*,20) i*h, y1(i), y2(i)
    end do
!    do level = 1, 3
!        do 
!            call numerov(y1, ksquare1, s, h)
!            call numerov(y2, ksquare2, s, h)
!            if (abs(y1(1000) - yend) < tol) then
!                y(level, 0:1000) = [(y1(i), i = 0,1000)]
!                ksquare = ksquare1 + 0.1
!                exit
!            end if
!            if (abs(y2(1000) - yend) < tol) then
!                y(level, 0:1000) = [(y2(i), i = 0,1000)]
!                ksquare = ksquare2 + 0.1
!                exit
!            end if
!            if (y1(1000)*y2(1000) < 0.) then
!                if (abs(y1(1000) - yend) < abs(y2(1000) - yend)) then
!                    ksquare1 = 0.5*(ksquare1 + ksquare2)
!                else
!                    ksquare2 = 0.5*(ksquare1 + ksquare2)
!                end if
!            else
!                ksquare1 = ksquare1 + 1.
!                ksquare2 = ksquare2 + 1.
!            end if
!        end do
!        ksquare1 = ksquare
!        ksquare2 = ksquare + 1.
!        y1(2:1000) = 10.0
!        y2(2:1000) = 10.0
!    end do
    
!    do i = 0, 1000
!        write(*,10) i*h, (y(level,i), level = 1,3)
!    end do
    
end program trialsubnumerov

include "sub_numerov.f95"
