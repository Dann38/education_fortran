program power_method
    use step_power_method
    implicit none
    
    integer, parameter :: file_1 = 1
    integer:: n, iteration, i, j
    real, allocatable, dimension(:, :) :: mtrx
    real, allocatable, dimension(:) :: eigenvector
    real :: eigenvalue

    iteration = 12

    !=================== read matrix ====================
    open (file_1, file = 'data.txt')
    read (file_1, *) n

    allocate(mtrx(n, n))
    read (file_1, *) mtrx
    allocate(eigenvector(n))
    close(file_1)                                                       !FILE CLOSE
    !====================================================

    call power_iteration(mtrx, n, iteration, eigenvector, eigenvalue)


    !=================== PRINT ==========================
    print '(a, i1, a)', '======== eigenvalues ', 1, '==========='
    print '(a, i1, a, f10.5)', 'a_', 1, ' = ', eigenvalue
    print *, ''
    print '(a, i1, a)', 'x^', 1, ' = '
    
    do j = 1, n
        print '(f7.3)', eigenvector(j)
    end do
    !====================================================


    !================== clean memory ====================
    deallocate(mtrx)
    deallocate(eigenvector)
    !====================================================
end program power_method
