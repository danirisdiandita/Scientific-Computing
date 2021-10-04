
!numerov subroutine. We should input three kinds of array
!1) y as the result of iteration
!2) ksquare as one part of the input
!3) s as one part of the input that makes the equation is not homogeneous
! we should insert the size of array
! from the usual diffential equation y" + ksquare(x) y = s(x)
! ksquare stands for k^2(x) and S stands for s(x)
subroutine numerov(y, ksquare, s, h)

    implicit none
    real*8, dimension(0:1000), intent(in) :: s, ksquare
    real*8, dimension(0:1000), intent(inout) :: y
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

end subroutine numerov
