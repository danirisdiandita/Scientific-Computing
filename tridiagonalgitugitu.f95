!program tridiagonalgitugitu
!    implicit none
!    real, parameter :: pi = 4.*atan(1.)
!    real, external :: polipolinom
!    integer :: n, i
!    real :: lambda, Ann, Annmin1, tolerance, awalan, dlambda
!    n = 30 ; lambda = -0.; Ann = -2.; Annmin1 = 1.; tolerance = 1.e-6
!    dlambda = -2.
!    awalan = polipolinom(n, lambda, Ann, Annmin1)
!    do
!        print*, lambda, polipolinom(n, lambda, Ann, Annmin1)
!        if (abs(polipolinom(n, lambda, Ann, Annmin1)) < tolerance) then
!            exit
!        end if
!        lambda = lambda + dlambda
!        if (polipolinom(n, lambda, Ann, Annmin1)*awalan < 0.) then
!            lambda = lambda - dlambda; dlambda = dlambda*0.5
!        end if
!    end do
!    print*, "end here"
!    do i = 1, 100
!        print*, "the lambda", lambda, -4.*sin(i*pi/(2*(n+1)))*sin(i*pi/(2*(n+1)))
!        if  (lambda + 4.*sin(i*pi/31.)*sin(i*pi/31.) < tolerance) then
!            print*, "the lambda", lambda, -4.*sin(i*pi/(2*(n+1)))*sin(i*pi/(2*(n+1)))
!            exit
!        end if
!    end do
!    print*, "real result", polipolinom(n, -1.02613540E-02, Ann, Annmin1)
    
!end program tridiagonalgitugitu
program ngegraph

    implicit none
    real, external :: polipolinom
    real :: lambda, a, b, h
    integer :: i, n = 1000
    a = -5. ; b = 5. ; h = (b - a)/real(n)
    do i = 0, 1000
        lambda = a + i*h
        write(*,"(2e14.6)") lambda, polipolinom(30, lambda, -2., 1.) 
    end do
end program ngegraph
real function polipolinom(n, lambda, Ann, Annmin1)
    implicit none
    real, intent(in) :: lambda, Ann, Annmin1
    integer, intent(in) :: n
    integer :: i
    real :: pertamax, keduax, Pn
    pertamax = Ann - lambda; keduax = (Ann - lambda)*pertamax - Annmin1*Annmin1
    do i = 3, n
        Pn = (Ann-lambda)*keduax - Annmin1*Annmin1*pertamax
        pertamax = keduax
        keduax = Pn
    end do
    polipolinom = Pn
end function polipolinom
