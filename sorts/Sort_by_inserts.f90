program sort_by_inserts
    implicit none

    integer, parameter :: len_array = 10
    integer :: array(len_array)
    integer :: i, j, memory

    array = [0, 9, 1, 2, 4, 3, 8, 7, 6, 5]

    print *, array

    do i = 2, len_array
        j = i-1
        memory = array(i)
        do while((j >= 0 ) .AND. (array(j) > memory)) 
            array(j+1) = array(j) 
            j = j - 1
        end do
        array(j+1) = memory
    end do

    print *, array


end program sort_by_inserts