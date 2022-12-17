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

    ! Test #3
    call add_test(test_invalid())
    call add_test(test_main())
    call add_test(test_bracket_space())

    ! Test #2
    call add_test(run_test2())

    if (nfailed<=0) then
        print *, 'SUCCESS! all tests passed.'
        stop 0
    else
        print *, 'ERROR: ',nfailed,' tests failed, ',npassed,' passed.'
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

       integer :: idx,ln
       character(*), parameter :: text = 'table football'

       idx = REGEX(string=text,pattern='foo*',length=ln);

       ! Prints "football"
       success = idx>0; if (.not.success) return
       success = text(idx:idx+ln-1) == "foo"

    end function test_main

    logical function test_bracket_space() result(success)
       use regex_module
       implicit none

       integer :: idx,ln
       character(*), parameter :: text = 'table football'

       idx = REGEX(string=text,pattern='e[ ]f',length=ln);

       ! Prints "football"
       success = idx>0;  if (.not.success) return
       success = text(idx:idx+ln-1) == "e f"

    end function test_bracket_space




end program tests
