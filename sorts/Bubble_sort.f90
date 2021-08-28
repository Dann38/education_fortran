program bubble_sort
    implicit none
    integer, parameter :: len_array = 10
    integer :: memory, i
    integer :: array(len_array)

    logical :: swapped 
    swapped = .TRUE.

    array = [0, 9, 1, 2, 4, 3, 8, 7, 6, 5]

    print *, array

    do while(swapped)
        swapped = .FALSE.
        do i = 1,len_array - 1
            if (array(i) > array(i+1) ) then
                    memory = array(i)
                    array(i) = array(i+1)
                    array(i+1) = memory
                    swapped = .TRUE.
            end if
        end do
    end do

    print *, array

    print *, 'ok'
end program bubble_sort