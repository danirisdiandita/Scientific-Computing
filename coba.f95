program coba
    
    real, dimension(10) :: y
    integer :: i
    y = 10.0
    print*, (y(i), i = 1,10)
    y = y + 0.1
    print*, (y(i), i = 1,10)

end program coba
