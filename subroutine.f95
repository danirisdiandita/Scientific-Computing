


program tukar
    implicit none
    integer, dimension(0:10):: c, d
    integer :: i

    c = [(i, i = 0, 10)]
    d = [(2*i, i = 0, 10)]
    write(*,10) "initially c = ", (c(i),i = 0,10)
    write(*,10) "initially d = ", (d(i),i = 0,10)
    
    call swap(c,d)
    print*, "the size of a is  ", size(c)
    write(*,10) "c = ", (c(i),i = 0,10)
    write(*,10) "d = ", (d(i),i = 0,10)
10  format(a16, 11i4)
end program tukar

subroutine swap(a,b)

!    implicit none
    integer, dimension(0:10), intent(inout) :: a, b
    integer, dimension(size(a)) :: work
    work = a
    a = b
    b = work

end subroutine swap
