program tests
    use regex_module
    use regex_test_1
    use regex_test_2
    use iso_fortran_env, only: output_unit
    implicit none

    integer :: nfailed = 0
    integer :: npassed = 0

    integer :: i,length
    logical :: valid
    character(len=30) :: pattern,str


    ! Test #1
    do i=1,size(test1data,2)
       call get_test1(i,valid,pattern,str,length)
       call add_test(run_test1(valid,pattern,trim(str),length))
    end do

    ! Test #2
    call add_test(run_test2())




    contains

    subroutine add_test(successful_test)
        logical, intent(in) :: successful_test
        if (successful_test) then
            npassed = npassed+1
        else
            nfailed = nfailed+1
        end if
    end subroutine add_test




end program tests
