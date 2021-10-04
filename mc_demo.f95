program mc_demo

  implicit none
  integer, parameter :: n = 1000
  call random_seed()!needed to initialize the random number generator used in MC eval
  call mc_integration(n,3.0)
  

end program mc_demo

subroutine mc_integration(n, end_val)
  implicit none
  integer, intent(in) :: n
  real, intent :: end_val
  real :: x, integral, integral_err
  real(kind=8) :: f, f2
  integer :: i
  integral = 0.0
  f = 0.0d0
  f2 = 0.0d0
  do i = 1, n
    call random_number(x)
    x = x*end_val

    f = f + integrand(x)
    f2 = f2 + (integrand(x)**2)
  end do

  f = f/n
  f2 = f2/n

  integral = (end_val-0.0)*f
  integral_err = (end_val-0.0)*SQRT((f2-f**2)/n)
  write(*,*) "#MC integration = ", integral, "+/-",





end subroutine mc_integration
