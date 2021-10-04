
program ndobel
    
    implicit none
    character (len =*), parameter :: a = 'just a simple test'
    print*, double(a)
contains
    function double(a)
        character (len=*), intent(in) :: a
        character (len = 2*len(a)) :: double
        double = a//a
    end function double
end program ndobel


