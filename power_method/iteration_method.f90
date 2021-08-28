module step_power_method
    implicit none
    
contains
    subroutine power_iteration(A, n, iteration, eigenvector, eigenvalue)
        implicit none
        integer, intent(in) :: n, iteration
        real, dimension(n, n) :: A
        real, intent(inout), dimension(n) :: eigenvector
        ! real, dimension(n) :: eigenvector_temp
        real, intent(inout) :: eigenvalue
        integer :: i
    

        
        call random_number(eigenvector)
        do i = 1, iteration
            eigenvector = matmul(A, eigenvector)
            eigenvector = eigenvector / norm2(eigenvector)
        enddo

        eigenvalue = dot(n,eigenvector,  matmul(A, eigenvector) )/ dot(n,eigenvector, eigenvector)

    end subroutine power_iteration

    function dot(n, x, y) result(retval)
        implicit none
        integer :: n, i
        real, dimension(n):: x, y
        real :: retval
        retval = 0 
        do i= 1, n
            retval = retval + x(i)*y(i)
        enddo 
        
    end function dot
end module step_power_method