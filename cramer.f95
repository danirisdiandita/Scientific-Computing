program crammer
    
    implicit none
    real*8, dimension(3,3) :: a, b, c
    integer :: i, j
    a = reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/))
    b = reshape((/1,0,0,0,1,0,0,0,1/),(/3,3/))
    c = matmul(a,b)
    10 format(3f14.6)
    do i = 1, 3
        write(*,10) (c(i,j), j = 1,3) 
    end do
end program crammer
