!inputs

!a = initial x
!b = final x
!n = steps
!h = stepsize = (b-a)/n
!x = array of x

!s_array = s array
!psileft0 = 
!psileft1 = 
!psirightN = 
!psirightnmin = 

!changing variables 

!E, k2_array = array of ksquare array

!outputs

!psileft = array
!psileft = array

!start algorithm
!a = -5.0 !ENOUGH YA
!b = 5.0
!n = 1000
!h =( b - a)/n
!E = 0.1
!x = array input a, b, h
!psileft0 = 0.0
!psileft1 = 0.1
!psirightN = 0.0
!psirightnmin = 0.1
!dE = 1.0
!(1)making a loop
!    (numerov algorithm with inputs x, karray, psileft0, psileft1
!    and get the outputs array of psileft(an array)) 
!    (numerov algorithm  with inputs x, karray, psirightN, psirightnmin
!    and get the outputs array of psiright(an array))
!    CONDITION = if the psileft function on the right < tolerance and psiright function on the left < tolerance
!        if both of them at point 0. THEN |psileft(0) - psiright(0)| < tolerance ==> we get the result 
!        ELSE CONDITION if psileft(end point) * psileft(end point) < 0 THEN dE = dE/2., E = E + dE
!        ELSE E = E + dE
!    make array of k with new E
!    
!(1) loop
program harmonicgitu

    implicit none
    real :: a, b, h, E, tolerance, endpointleft, startpointright, dE
    integer :: i
    integer,  parameter :: n = 1000
    real, dimension(0:n) :: x, psileft, psiright, k2_array, s_array 
    real :: psileft0, psileft1, psirightN, psirightnmin
    real, external :: k2
    a = -5. ; b = 5.; h = (b-a)/real(n)
    x = (/(a+i*h, i = 0, n)/)
    psileft0 = 0.0; psileft1 = 0.1; psirightN = 0.0; psirightnmin = 0.1
    E = 0.1; dE = 1.0
    psileft(0) = psileft0; psileft(1) = psileft1
    psiright(n) = psirightN; psiright(n-1) = psirightnmin
    tolerance = 1.e-6
    s_array = 0.0
    k2_array = [(k2(E, x(i)), i = 0, n)]
    do i = 1, n
        psileft(i+1) = (2.*(1. - (5./12.)*h*h*k2_array(i)) &
        & *psileft(i) - (1.+ h*h*(1./12.)*k2_array(i-1))  &
        & *psileft(i-1)+h*h*(1./12.)*(s_array(i+1) + &
        & 10.*s_array(i) + s_array(i-1)))/ &
        & (1.+h*h*(1./12.)*k2_array(i+1))
    end do
    do i = n, 1, -1
        psiright(i-1) = (2.*(1. - (5./12.)*h*h*k2_array(i)) &
        & *psiright(i) - (1.+ h*h*(1./12.)*k2_array(i+1))  &
        & *psiright(i+1)+h*h*(1./12.)*(s_array(i+1) + &
        & 10.*s_array(i) + s_array(i-1)))/ &
        & (1.+h*h*(1./12.)*k2_array(i-1))
    end do
    
    
    
    endpointleft = psileft(n); startpointright = psiright(0)
!=================================================================    
!     THE START OF LONG LOOP
!=================================================================
!    do
!        write(*,*) E
!        if (abs(endpointleft) < tolerance .and. abs(startpointright) < tolerance) then
!            if (abs(psileft(0)-psiright(0)) < tolerance) exit
!        end if
!        E = E + dE
!        k2_array = [(k2(E, x(i)), i = 0, n)]
!        do i = 1, n
!            psileft(i+1) = (2.*(1. - (5./12.)*h*h*k2_array(i)) &
!            & *psileft(i) - (1.+ h*h*(1./12.)*k2_array(i-1))  &
!            & *psileft(i-1)+h*h*(1./12.)*(s_array(i+1) + &
!            & 10.*s_array(i) + s_array(i-1)))/ &
!            & (1.+h*h*(1./12.)*k2_array(i+1))
!        end do
!        do i = n, 1, -1
!            psiright(i-1) = (2.*(1. - (5./12.)*h*h*k2_array(i)) &
!            & *psiright(i) - (1.+ h*h*(1./12.)*k2_array(i+1))  &
!            & *psiright(i+1)+h*h*(1./12.)*(s_array(i+1) + &
!            & 10.*s_array(i) + s_array(i-1)))/ &
!            & (1.+h*h*(1./12.)*k2_array(i-1))
!        end do
!        if (psileft(n)*endpointleft < 0) then
!            dE = 0.5*dE
!            E = E - dE
!        end if
!    end do
!==================================================================
!   END OF THE LOOOP
!==================================================================
    
!=======================
!   WRITING THE RESULT
!=======================
    do i = 0, n
        write(*,10) x(i), psileft(i), psiright(i)
    end do
    10 format(3e14.6)
end program harmonicgitu 

!define the function of ksquare
real function k2(E, x)
    
    implicit none
    real, intent(in) :: E, x
    k2 = 2.*(E - 0.5*x*x)
    
end function k2
