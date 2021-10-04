
program integration_with_simpson

    implicit none
    real*8 :: integral, a, b
    real*8, external :: func
    a = 0.0; b = 1.0
    call simpson(a, b, func, integral)
    print *, integral
end program integration_with_simpson

real*8 function func(x)
    implicit none
    real*8, intent(in) :: x
    func = x*x
end function func

subroutine simpson(a, b, f, N, integral)

    implicit none
    real*8 :: a, b, h
    integer, intent(in) :: i, N
    real*8, external, intent(in) :: f
    real*8, intent(out) :: integral
    real*8, dimension(0:N) :: part
    h = ( b - a ) / real(N)
    part(0:N) = [(f(i*h), i = 0,N)]
    integral = 0.0 !start the iteration
    !the f0
    integral = integral + part(0) + part(N)
    !the odd part
    do i = 1, N/2
        integral = integral + 4*part(2*i - 1)
    end do
    !the even part
    do i = 1, (N/2) - 1
        integral = integral + 2*part(2*i)
    end do
    integral = (h/3.)*integral
end subroutine simpson
