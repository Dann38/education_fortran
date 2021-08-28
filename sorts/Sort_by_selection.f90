program sort_by_selection
    implicit none

    integer, parameter :: len_array = 10

    integer :: array(len_array)
    integer :: i, j, memory

    array = [0, 9, 1, 2, 4, 3, 8, 7, 6, 5]

    print *, array

    do i = 1, len_array
        do j = i+1, len_array
            if ( array(j) < array(i) ) then
                memory = array(i)
                array(i) = array(j)
                array(j) = memory
            end if
        end do
    end do

    print *, array
    
end program sort_by_selection