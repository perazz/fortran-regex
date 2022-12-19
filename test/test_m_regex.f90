! Some tests from https://github.com/urbanjost/M_match
module regex_test_m_regex
    use regex_module
    public

    ! pattern, string, index>0
    character(len=*,kind=RCK), parameter :: testMdata(3,91) = reshape([ character(len=30) :: &
        "Foo",        "FooBar",        "YES", &
        "Poo",        "FooBar",        "NO " , &
        "Bar",        "FooBar",        "YES", &
        "Par",        "FooBar",        "NO " , &
        "Foo",        "Foo",           "YES", &
        "Fo",         "Foo",           "YES", &
        "Foo",        "Fo",            "NO " , &
        "ooB",        "FooBar",        "YES", &
        "ooP",        "FooBar",        "NO " , &
        ".",          "FooBar",        "YES", &
        "P.",         "FooBar",        "NO " , &
        "^Foo",       "FooBar",        "YES", &
        "^Bar",       "FooBar",        "NO " , &
        "Foo$",       "FooBar",        "NO " , &
        "Bar$",       "FooBar",        "YES", &
        ".*o",        "FooBar",        "YES", &
        "o*o",        "FooBar",        "YES", &
        "P*o",        "FooBar",        "YES", &
        "Fo*o",       "FooBar",        "YES", &
        "Po*o",       "FooBar",        "NO " , &
        "F[po]o",     "FooBar",        "YES", &
        "F[op]o",     "FooBar",        "YES", &
        "F[qp]o",     "FooBar",        "NO " , &
        "F[^po]o",    "FooBar",        "NO " , &
        "F[^op]o",    "FooBar",        "NO " , &
        "F[^qp]o",    "FooBar",        "YES", &
        "F[po]*o",    "FooBar",        "YES", &
        "F[56]*o",    "F5oBar",        "YES", &
        "F[46]*o",    "F5oBar",        "NO " , &
        "F[46]*5",    "F5oBar",        "YES", &
        "F[46]*5o",   "F5oBar",        "YES", &
        "F[op]*o",    "FooBar",        "YES", &
        "F[qp]*o",    "FooBar",        "YES", &
        "P[qp]*o",    "FooBar",        "NO " , &
        "F[^po]*o",   "FooBar",        "YES", &
        "F[^op]*o",   "FooBar",        "YES", &
        "F[^qp]*o",   "FooBar",        "YES", &
        "P[^qp]*o",   "FooBar",        "NO " , &
        "[0-9][0-9]*$",  "0123456789",  "YES" , &
        "[0-9][0-9]*$",  "A0123456789", "YES" , &
        "^[0-9][0-9]*$", "A0123456789", "NO ", &
        "^[0-9][0-9]*$", "",            "NO ", &
        "^[0-9]$", "",                  "NO ", &
        "^[0-9]*$", "",                 "YES" , &
        "^$", "",                        "YES", &
        "^$", " ",                      "NO ", &
        "^[A-Z ][A-Z ]*$", "",          "NO ", &
        "^[ ]*[A-Z][A-Z ]*$", " THIS IS ALL UPPERCASE",    "YES", &
        "^[ ]*[a-z][a-z ]*$", " this is all lowercase",    "YES", &
        "^[ ]*[A-Z][A-Z ]*$", " THIS IS not ALL UPPERCASE",    "NO " , &
        "^[ ]*[a-z][a-z ]*$", " this is NOT all lowercase",    "NO " , &
        "X[-+]Y", "X-Y",                        "YES", &
        "X[-+]Y", "X+Y",                        "YES", &
        "X[+-]Y", "X-Y",                        "YES", &
        "X[+-]Y", "X+Y",                        "YES", &
        "X[-+]Y", "Y-X",                        "NO ", &
        "X[-+]Y", "Y+X",                        "NO ", &
        "X[+-]Y", "Y-X",                        "NO ", &
        "X[+-]Y", "Y+X",                        "NO ", &
        "X"//TAB//"Y", "X"//TAB//"Y",             "YES", &
        "X["//TAB//"ab]Y", "X"//TAB//"Y",         "YES", &
        "X["//TAB//"ab]Y", "XtY",                 "NO ", &
        "X["//TAB//"ab]Y", "XaY",                 "YES", &
        "[0-9][0-9]*\.[0-9]*",   "1.9",           "YES", &
        "[0-9][0-9]*\.[0-9]*",   "1.99",          "YES", &
        "[0-9][0-9]*\.[0-9]*",   "1.999",         "YES", &
        "[0-9][0-9]*\.[0-9]*",   "1.9999",        "YES", &
        "[0-9][0-9]*\.[0-9]*",   "1.99999",       "YES", &
        "[0-9][0-9]*\.[0-9]*",   "11.99999",      "YES", &
        "[0-9][0-9]*\.[0-9]*",   "111.99999",     "YES", &
        "[0-9][0-9]*\.[0-9]*",   "1111.99999",    "YES", &
        "[0-9][0-9]*\.[0-9]*",   "11111.99999",   "YES", &
        "[0-9][0-9]*\.[0-9]*",   "123456.99999",  "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "1.9",           "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "1.99",          "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "1.999",         "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "1.9999",        "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "1.99999",       "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "11.99999",      "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "111.99999",     "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "1111.99999",    "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "11111.99999",   "YES", &
        "^[0-9][0-9]*\.[0-9]*",  "111111.99999",  "YES", &
        "a[0-9][0-9]*\.[0-9]*",  "a1.9",          "YES", &
        "a[0-9][0-9]*\.",        "a1.9",          "YES", &
        "a[0-9][0-9]*",          "a1.9",          "YES", &
        "a",                "a1.9",          "YES", &
        "\\",               "\",             "YES", &
        "\.",               "\",             "NO " , &
        ".",                "\",             "YES", &
        "F[qpo", "FooBar", "NO "],[3,91])

        ! These cases have C-specific characters and need be defined

    contains

    subroutine get_m_test(itest,valid,pattern,string)
        integer, intent(in) :: itest
        logical, intent(out) :: valid
        character(*), intent(out) :: pattern,string

        if (.not.(itest>0 .and. itest<=size(testMdata,2))) return

        valid   = trim(testMdata(3,itest))=='YES'
        pattern = testMdata(1,itest)
        string  = testMdata(2,itest)

    end subroutine get_m_test

    logical function run_m_test(valid,pattern,string) result(success)
       logical,      intent(in) :: valid
       character(*), intent(in) :: pattern
       character(*), intent(in) :: string

       integer :: idx,length
       type(regex_op) :: re

       print "('regex test: pattern=',a,' string=',a,'....')",trim(pattern),trim(string)

       idx = regex(string, pattern, length)

       ! This test does not check the length of the match
       if (.not.valid) then
          success = idx<=0
       else
          success = idx>0 .or. (idx==0 .and. len(string)==0)
       end if

       if (.not.success) then
          write(*,*) 'FAILED: regex result: idx=',idx,' length=',length,' expected valid = ',valid
          re = parse_pattern(pattern)
          print *, '    ...pattern breakdown: '
          call re%write()
       endif

    end function run_m_test


end module regex_test_m_regex
