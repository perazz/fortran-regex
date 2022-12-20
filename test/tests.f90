program tests
    use regex_module
    use regex_test_1
    use regex_test_2
    use regex_test_m_regex
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

    ! Test m_regex
    do i=1,size(testMdata,2)
        call get_m_test(i,valid,pattern,str)
        call add_test(run_m_test(valid,trim(pattern),trim(str)))
    end do

    ! Test #3
    call add_test(test_invalid())
    call add_test(test_main())
    call add_test(test_bracket_space())
    call add_test(test_end_anchor())
    call add_test(test_end_anchor2())

    ! Test #2
    call add_test(run_test2())

    if (nfailed<=0) then
        print "(*(a,:,i0))", 'SUCCESS! all ',npassed,' tests passed.'
        stop 0
    else
        print "(*(a,:,i0))", 'ERROR: ',nfailed,' tests failed, ',npassed,' passed.'
        stop 1
    end if


    contains

    subroutine add_test(successful_test)
        logical, intent(in) :: successful_test
        if (successful_test) then
            npassed = npassed+1
        else
            nfailed = nfailed+1
        end if
    end subroutine add_test

    ! Test two bug patterns reported by @DavidKorczynski in https://github.com/kokke/tiny-regex-c/issues/44
    logical function test_invalid() result(success)

       type(regex_op) :: re

       ! Test 1: inverted set without a closing ']'
       re = parse_pattern("\\\x01[^\\\xff][^")
       success = re%n==0; if (.not.success) return

       ! Test 1: inverted set without a closing ']'
       re = parse_pattern("\\\x01[^\\\xff][\\")
       success = re%n==0; if (.not.success) return

    end function test_invalid

    logical function test_main() result(success)
       use regex_module
       implicit none

       character(*), parameter :: text = 'table football'

       success = check_pattern(text,'foo*',expected="foo")
       if (.not.success) return

    end function test_main

    logical function test_bracket_space() result(success)
       use regex_module
       implicit none

       character(*), parameter :: text = 'table football'

       success = check_pattern(text,'e[ ]f',expected="e f")
       if (.not.success) return

       success = check_pattern(text,'e[ ]+f',expected="e f")
       if (.not.success) return


    end function test_bracket_space

    logical function test_end_anchor() result(success)
       use regex_module
       implicit none

       character(*), parameter :: text = 'table football'

       success = check_pattern(text,'ll$',expected="ll")
       if (.not.success) return

       success = check_pattern(text,'l$',expected="l")
       if (.not.success) return

    end function test_end_anchor

    logical function test_end_anchor2() result(success)
       use regex_module
       implicit none

       character(*), parameter :: text = 'Avida Dollar$'

       success = check_pattern(text,'[A-Z][a-z]+$',expected="")
       if (.not.success) return

    end function test_end_anchor2

end program tests
