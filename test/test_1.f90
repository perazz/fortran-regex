program tests
    use regex
    use regex_testdata
    use iso_fortran_env, only: output_unit
    implicit none

    integer :: nfailed = 0
    integer :: npassed = 0

    integer :: i,length
    logical :: valid
    character(len=30) :: pattern,str

    ! Read results file
    ! call read_results

    ! Perform all tests
    do i=1,size(test1data,2)
       call get_test1(i,valid,pattern,str,length)

       call add_test(test1(valid,pattern,str,length))

       if (nfailed>0) stop 'test failed'

    end do



    contains

    subroutine add_test(successful_test)
        logical, intent(in) :: successful_test
        if (successful_test) then
            npassed = npassed+1
        else
            nfailed = nfailed+1
        end if
    end subroutine add_test

    logical function test1(valid,pattern,str,correct_len) result(success)
       logical,      intent(in) :: valid
       character(*), intent(in) :: pattern
       character(*), intent(in) :: str
       integer,      intent(in) :: correct_len


       integer :: length

       type(regex_op) :: reg

       call reg%parse(pattern)

       write(*,*) 'pattern = ',trim(pattern)
       call reg%write(output_unit)

       success = .true.


    end function test1

end program tests
