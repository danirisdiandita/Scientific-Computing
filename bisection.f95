program rootfinding

    implicit none
    real*8 :: tolx, x, fold, dx
    real*8, external :: f
    integer :: iter
    tolx = 1.e-12
    x = -1.0
    fold = f(x)
    dx = 0.5
    iter = 0
    do while(abs(dx) .gt. tolx)
        iter = iter + 1
        x = x + dx
        print*, iter, x, f(x)
        if ((fold*f(x)) .lt. 0) then
            x = x - dx
            dx = dx/2
        end if
    end do

end program rootfinding

real*8 function f(x)
    real*8, intent(in) :: x
    f = x*x - 5.
end function f
