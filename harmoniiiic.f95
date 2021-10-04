program harmonic

    implicit none
    integer :: n, i
    real*8, dimension(-n/2,n/2) :: y, ksquare, s
    real*8 :: a, b, h
    interface
        subroutine forwardnumerov(y, ksquare, s, h)
        real*8, dimension(:):: s, ksquare, y
        end subroutine forwardnumerov
    end interface
    a = -5.0; b = 5.0; h = (a-b)/real(n)
    ksquare = 1.
    y(-n/2) = 0.0
    y(-n/2 + 1) = 1.0
    s = 0.0
    call forwardnumerov(y, ksquare, s, h)
    do i = -n/2, n/2
        write(*,*) a + i*h, y(i)
    end do
end program harmonic
 
subroutine forwardnumerov(y, ksquare, s, h)

    implicit none
    real*8, dimension(:), intent(in) :: s, ksquare
    real*8, dimension(:), intent(inout) :: y
    real*8 , intent(in) :: h
    real*8 :: left, middle, right, down
    integer :: i
    do i = 1, size(y) - 1
        left = (1./12.)*h*h*(s(i+1) +10.*s(i)+s(i-1))
        middle = 2.*(1.-(5./12.)*h*h*ksquare(i))*y(i)
        right = (1.+(1./12.)*h*h*ksquare(i-1))*y(i-1)
        down = 1.+(1./12.)*h*h*ksquare(i+1)
        y(i+1) = (left+middle-right)/down
    end do

end subroutine forwardnumerov

real*8 function k2(E, x)

    implicit none
    real*8, intent(in) :: E, x
    k2 = 2.*(E - 0.5*x*x)

end function k2
