program harmonicoscillator
    implicit none
    real :: a, b, psileft0, psileft1, h, E
    integer, parameter :: n = 100
    integer :: i
    real, external :: k
    real, dimension(n+1) :: x, psileft, psiright, k2array, s_array
    interface
        subroutine forwardnumerov(x,y, y0, y1, k2array, s_array)
        real, dimension(:) :: x,y,k2array, s_array
        end subroutine forwardnumerov
    end interface
    a = -5.
    b = 5.
    h = (b-a)/real(n)
    x = (/(a + i*h, i = 0, n)/)
    psileft0 = 0.
    psileft1 = 0.1
    E = 0.3
    k2array = (/(k(E, x(i)), i = 1, n + 1)/)
    s_array = 0.0
    call forwardnumerov(x,psileft,psileft0, psileft1, k2array, s_array)
    do i = 1, n+1
        write(*,10) x(i) , psileft(i)
    end do
    10 format(2e14.6)
end program harmonicoscillator
real function k(E, x)
    
    implicit none
    real, intent(in) :: E, x
    k = 2.*(E - 0.5*x*x)
    
end function k  

subroutine forwardnumerov(x,y, y0, y1, k2array, s_array)

    implicit none
    real, dimension(:), intent(in) :: x, k2array, s_array
    real, dimension(:), intent(out) :: y
    real :: y0, y1 
    integer :: i
    real :: left, middle, right, down, h
    y(1) = y0
    y(2) = y1
    h = x(2) - x(1)
    do i = 2, size(x) - 1
        left = 2.*(1.-(5./12.)*k2array(i))
        middle = 1.+(h*h/12.)*k2array(i-1)
        right = h*h/12.
        down = 1.+(h*h/12.)*k2array(i+1)
        y(i+1) = (left*y(i) - middle*y(i-1)+right*(s_array(i+1) +10.*s_array(i)+s_array(i-1)))/down
    end do
    
end subroutine forwardnumerov
