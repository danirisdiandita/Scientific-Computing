program gaussianquadrature
    
    implicit none
    real, external :: qgauss, fungsi
    print*, qgauss(fungsi, 0., 1., 100)
end program gaussianquadrature



real function fungsi(x)

    implicit none
    real, intent(in) :: x
    fungsi = x
    
end function fungsi
include "Gaussm3.f90"
