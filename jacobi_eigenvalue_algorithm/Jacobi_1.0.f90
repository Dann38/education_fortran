program Jacobi
    !The exit is marked with label 1
    implicit none

    integer, parameter :: file_1 = 1, file_2 = 2
    integer:: n
    real, allocatable, dimension(:, :) :: mtrx, S, mtrx_2
    logical :: run
    integer :: i, j, k, eta, i0, j0
    real :: tg_2fi, K_, cos_fi, sin_fi, epsilon_
    
    epsilon_ = 1.0e-2
    

    
    !=================== read and create matrix ====================
    open (file_1, file = 'data.txt')
    read (file_1, *) n

    allocate(mtrx(n, n))
    read (file_1, *) mtrx
    close(file_1)                                                       !FILE CLOSE

    allocate(mtrx_2(n, n))
    
    allocate(S(n, n))
    S = 0
    do i = 1,n
        S(i, i) = 1
    end do

    
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    
    !=================== test symetric =============================
    do i = 1, n
        do j = 1, n
            if (mtrx(i, j) /=  mtrx(j, i)) then
                print *, 'It is not symmetric'
                go to 1 !for exit
            end if
        end do
    end do
    print *, 'It is symmetric'
    
    print *, '======== data =========='
    print "("//repeat( "f7.3, ", n-1)//"f7.3) ", mtrx
    print *, ''
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    
    run = .TRUE.
    do while (run)
    !================ one step metod ===============================
    run = .FALSE.
    i0 = 2
    j0 = 1
    do i = 2,n
        do j = 1, (i-1)
            if (abs(mtrx(i0, j0)) < abs(mtrx(i, j))) then
                i0 = i
                j0 = j
            end if
            if (abs(mtrx(i, j)) > epsilon_) then
                run = .TRUE.
            end if
        end do
    end do
    
    tg_2fi = 2 * mtrx(i0, j0)/(mtrx(i0, i0)-mtrx(j0,j0))

    if (tg_2fi < 0.0) then
        eta = -1.0
    else if (tg_2fi > 0.0)  then
        eta = 1.0
    else
        go to 1 !exit
    end if

    K_ = 1.0/sqrt(1+tg_2fi**2)
    sin_fi = eta*sqrt((1.0-K_)/2)
    cos_fi = sqrt((1.0+K_)/2)

    mtrx_2 = mtrx
    do k = 1, n
        mtrx(k, i0) = mtrx_2(k, i0)*cos_fi + mtrx_2(k, j0)*sin_fi
        mtrx(k, J0) = -mtrx_2(k, i0)*sin_fi + mtrx_2(k, j0)*cos_fi
    end do

    mtrx_2 = mtrx
    do k = 1, n
        mtrx(i0, k) = mtrx_2(i0, k)*cos_fi + mtrx_2(j0, k)*sin_fi
        mtrx(j0, k) = -mtrx_2(i0, k)*sin_fi + mtrx_2(j0, k)*cos_fi
    end do

    
    mtrx_2 = 0
    do i = 1,n
        mtrx_2(i, i) = 1
    end do

    mtrx_2(i0, i0) = cos_fi
    mtrx_2(i0, j0) = -sin_fi
    mtrx_2(j0, i0) = sin_fi
    mtrx_2(j0, j0) = cos_fi

    S = matmul(S, mtrx_2)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    end do


    ! for write in file
    
    ! print "("//repeat( "f7.3, ", n-1)//"f7.3) ", mtrx

    do i = 1, n
        print '(a, i1, a)', '======== eigenvalues ', i, '==========='
        print '(a, i1, a, f10.5)', 'a_', i, ' = ', mtrx(i, i)
        print *, ''
        print '(a, i1, a)', 'x^', i, ' = '
        
        do j = 1, n
            print '(f7.3)', S(j, i)
        end do
    end do
    
    ! print "("//repeat( "f7.3, ", n-1)//"f7.3 ) ", S
    ! open (file_2, file = 'rez2.txt')!, status = 'new')
    ! write (file_2, "("//repeat( "f7.3, ", n-1)//"f7.3) ") mtrx

1   deallocate(mtrx)
    deallocate(mtrx_2)
    deallocate(S)
    !close(file_2)
end program Jacobi
