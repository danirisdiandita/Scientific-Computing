program harmonic_oscillator
!======================================================================
!   DECLARATIONS - JUST IGNORE THIS -
!======================================================================
    implicit none
    real*8 :: Energy, h, a, b
    real*8, parameter :: N = 1000
    real*8, dimension(-N:N), external :: knsquare, sn
    real*8, dimension(-N:N) :: psileft, psiright, kn2, snarray
    integer :: i
    interface
        function knsquare(Energy, h)
        real*8, dimension(:) :: knsquare, x
        end function knsquare
    end interface
    interface
        function sn()
        real*8, dimension(:) :: sn
        end function
    end interface
    interface
        subroutine forwardnumerov(y, knsquare, sn, h, N)
        real*8, dimension(:) :: knsquare, sn, y
        end subroutine forwardnumerov
    end interface
    interface 
        subroutine backwardnumerov(y, knsquare, sn, h, N)
        real*8, dimension(:) :: knsquare, sn, y
        end subroutine backwardnumerov
    end interface
    open(10, file = "psileft.txt")
    open(15, file = "psiright.txt")
20  format(2e14.6)
25  format(2e14.6)
!======================================================================
!   THE END OF DECLARATIONS
!======================================================================
!======================================================================
!
!   INITIAL CONDITIONS
!
!======================================================================
    a = -10.0
    b = 10.0
    h = (b - a)/real(N)
    x(:) = [(i*h, i = -N, N)]
    Energy = 0.0 !initially
    kn2 = knsquare(Energy, x)
    snarray = sn()
    psileft(-N) = 0.0; psileft(-N + 1) = 0.01
    psiright(N) = 0.0; psiright(N-1) = 0.01
    call forwardnumerov(psileft, kn2, snarray, h, N)
    call backwardnumerov(psiright, kn2, snarray, h, N)
!======================================================================
!
!   START THE LOOP
!
!======================================================================
    do i = -N, N
        write(10,20) i*h, psileft(i)
    end do
    do i = -N, N
        write(15,25) i*h, psiright(i)
    end do
!    do
!        kn2 = knsquare(Energy, x)
!        call forwardnumerov(psileft,kn2, sn, h, N)
!        call backwardnumerov(psiright,kn2, sn, h, N)
!        if (psileft(N) < tol)
!            exit
!        elseif (psileft(N)*)
!        end if 
!    end do

end program harmonic_oscillator

function knsquare(Energy, x)
    
    implicit none
    real*8, dimension(:), intent(in) :: x
    real*8, dimension(:) :: knsquare
    real*8, intent(in) :: Energy
    knsquare = 2.*(Energy - x*x)
    
end function knsquare

function sn()
    
    implicit none
    real*8, dimension(:) :: sn
    sn = 0.0

end function sn

subroutine forwardnumerov(y, knsquare, sn, h, N)
    
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: h
    real*8, dimension(:), intent(in) :: knsquare, sn
    real*8, dimension(:), intent(inout) :: y
    real*8 :: left, middle, right, down
    integer :: i
    do i = -N + 1, N - 1
        left = 2.*(1.+(5./12.)*h*h*knsquare(i))
        middle = 1.+h*h*(1./12.)*knsquare(i-1)
        right = h*h*(1./12.)*(sn(i+1) + 10.*sn(i) + sn(i-1))
        down = 1. + h*h*(1./12.)*knsquare(i+1)
        y(i+1) = (left*y(i) + middle*y(i-1) + right)/down
    end do

end subroutine forwardnumerov

subroutine backwardnumerov(y, knsquare, sn, h, N)
    
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) ::  h
    real*8, dimension(:), intent(in) :: knsquare, sn
    real*8, dimension(:), intent(inout) :: y
    real*8 :: left, middle, right, down
    integer :: i
    do i = N-1, -N + 1, -1
        left = 2.*(1.+(5./12.)*h*h*knsquare(i))
        middle = 1.+h*h*(1./12.)*knsquare(i+1)
        right = h*h*(1./12.)*(sn(i+1) + 10.*sn(i) + sn(i-1))
        down = 1. + h*h*(1./12.)*knsquare(i-1)
        y(i-1) = (left*y(i) + middle*y(i+1) + right)/down
    end do

end subroutine backwardnumerov
