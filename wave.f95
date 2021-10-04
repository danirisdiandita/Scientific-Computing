
program chap3a

    implicit none
    real*8, dimension(0:200) :: phi, r, source
    real*8 :: a, b, h, const, slope
    real*8, external :: S, exact
    integer :: i
    open(10, file = "wave.txt")
    20 format(3e14.5)
    a = 0.
    b = 20.
    h = (b - a)/200.
    const = h*h/12.
    r(0:200) = [(i*h, i = 0, 200)]
    source(0:200) = [(S(r(i)), i = 0,200)]
    phi(0) = 0.
    phi(1) = 4.9920711062242495E-002
    do i = 1,200
        phi(i+1) = 2.*phi(i)-phi(i-1) + const*(source(i+1)+10.*source(i)+source(i-1))
    end do
    
    slope = (phi(200) - phi(200-10))/(10*h)
    do i = 1, 200
        phi(i) = phi(i) - slope*i*h
    end do
    
    do i = 0, 200
    write(10,20) i*h, phi(i), phi(i) - exact(i*h)
    end do
end program chap3a

real*8 function S(r)
    
    implicit none
    real*8, intent(in) :: r
    S = -0.5*r*exp(-r)
    
end function S

real*8 function exact(r)

    implicit none
    real*8,intent(in) :: r
    exact = 1. - (r+2.)*exp(-r)/2.
    
end function exact


