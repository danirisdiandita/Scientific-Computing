!LI, Guodong
!______________________________________________________________________
!determinant of a matrix (fortran 90)
!function to find the determinant of a square matrix
!Author : Louisda16th a.k.a Ashwith J. Rego
!Description : the subroutine is based on two key points:
! A determinant is unaltered when row operations are performed : 
!Hence, using this principle, row operations (column operations would 
!work as well) are used
!to convert  the matrix into upper  triangular form 
!The determinant of a triangular matrix is obtained by finding the 
!product of the diagonal elements
!______________________________________________________________________


real function FindDet(matrix, n)
    implicit none
    real, dimension(n,n) :: matrix
    integer, intent(in) :: n
    real :: m, temp
    integer :: i, j, k, l
    logical :: DetExists = .true.
    l = 1
    !convert to upper triangular form
    do k = 1, n - 1
        if(matrix(k,k) == 0) then
        
    end do



end function FindDet
