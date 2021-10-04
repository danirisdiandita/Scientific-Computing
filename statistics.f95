program statistics

	implicit none
	integer, parameter :: n = 6
	real*8, dimension(n) :: seriesA, seriesB
	real*8 :: meanA, meanB
	real*8 :: variationA
	real*8 :: standard_deviationA
	real*8, external :: means, variation, standard_deviation
	
	
	seriesA = (/11,12,34,21,75,11/)
	seriesB = (/21,33,44,55,66,98/)
	
	meanA = means(seriesA, n)
	variationA = variation(seriesA, n)
	standard_deviationA = standard_deviation(seriesA, n)
	print*, meanA, variationA, standard_deviationA
	

end program

real*8 function means(x, n)

	implicit none
	integer, intent(in) :: n
	real*8, dimension(n) :: x
	
	means = sum(x) /real(n)

end function

real*8 function variation(x,n)

	implicit none
	integer, intent(in) :: n
	integer :: i
	real*8, dimension(n), intent(in) :: x
	real*8, external :: means
	variation = means([(x(i)*x(i), i = 1, n)], n) - (means(x,n))**2

end function

real*8 function standard_deviation(x, n)

	implicit none
	integer, intent(in) :: n
	real*8, dimension(n), intent(in) :: x
	real*8, external :: variation
	standard_deviation = variation(x,n)**0.5

end function
