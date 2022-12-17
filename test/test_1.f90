module regex_test_1
    use regex_module
    implicit none
    public

    ! Some tests from tiny-regex-c
    ! valid, pattern, string, length
    character(len=*,kind=RCK), parameter :: test1data(4,66) = reshape([ character(len=30) :: &
        "YES", "\d                           ", "5                             ", "1 ", &
        "YES", "\w+                          ", "hej                           ", "3 ", &
        "YES", "\s                           ", "\t \n                         ", "1 ", &
        "NO ", "\S                           ", "\t \n                         ", "0 ", &
        "YES", "[\s]                         ", "\t \n                         ", "1 ", &
        "NO ", "[\S]                         ", "\t \n                         ", "0 ", &
        "NO ", "\D                           ", "5                             ", "0 ", &
        "NO ", "\W+                          ", "hej                           ", "0 ", &
        "YES", "[0-9]+                        ", "12345                         ", "5 ", &
        "YES", "\D                           ", "hej                           ", "1 ", &
        "NO ", "\d                           ", "hej                           ", "0 ", &
        "YES", "[^\w]                        ", "\                            ", "1 ", &
        "YES", "[\W]                         ", "\                            ", "1 ", &
        "NO ", "[\w]                         ", "\                            ", "0 ", &
        "YES", "[^\d]                        ", "d                             ", "1 ", &
        "NO ", "[\d]                         ", "d                             ", "0 ", &
        "NO ", "[^\D]                        ", "d                             ", "0 ", &
        "YES", "[\D]                         ", "d                             ", "1 ", &
        "YES", "^.*\\.*$                    ", "c:\Tools                     ", "8 ", &
        "YES", "^[\+-]*[\d]+$               ", "+27                           ", "3 ", &
        "YES", "[abc]                         ", "1c2                           ", "1 ", &
        "NO ", "[abc]                         ", "1C2                           ", "0 ", &
        "YES", "[1-5]+                        ", "0123456789                    ", "5 ", &
        "YES", "[.2]                          ", "1C2                           ", "1 ", &
        "YES", "a*$                           ", "Xaa                           ", "2 ", &
        "YES", "a*$                           ", "Xaa                           ", "2 ", &
        "YES", "[a-h]+                        ", "abcdefghxxx                   ", "8 ", &
        "NO ", "[a-h]+                        ", "ABCDEFGH                      ", "0 ", &
        "YES", "[A-H]+                        ", "ABCDEFGH                      ", "8 ", &
        "NO ", "[A-H]+                        ", "abcdefgh                      ", "0 ", &
        "YES", "[^\s]+                       ", "abc def                       ", "3 ", &
        "YES", "[^fc]+                        ", "abc def                       ", "2 ", &
        "YES", "[^d\sf]+                     ", "abc def                       ", "3 ", &
        "YES", "\n                            ", "abc\ndef                      ", "1 ", &
        "YES", ".*c                           ", "abcabc                        ", "6 ", &
        "YES", ".+c                           ", "abcabc                        ", "6 ", &
        "YES", "[b-z].*                       ", "ab                            ", "1 ", &
        "YES", "b[k-z]*                       ", "ab                            ", "1 ", &
        "NO ", "[0-9]                         ", "-                             ", "0 ", &
        "YES", "[^0-9]                        ", "-                             ", "1 ", &
        "YES", "0|                            ", "0|                            ", "2 ", &
        "NO ", "\d\d:\d\d:\d\d          ", "0s:00:00                      ", "0 ", &
        "NO ", "\d\d:\d\d:\d\d          ", "000:00                        ", "0 ", &
        "NO ", "\d\d:\d\d:\d\d          ", "00:0000                       ", "0 ", &
        "NO ", "\d\d:\d\d:\d\d          ", "100:0:00                      ", "0 ", &
        "NO ", "\d\d:\d\d:\d\d          ", "00:100:00                     ", "0 ", &
        "NO ", "\d\d:\d\d:\d\d          ", "0:00:100                      ", "0 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "0:0:0                         ", "5 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "0:00:0                        ", "6 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "0:0:00                        ", "5 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "00:0:0                        ", "6 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "00:00:0                       ", "7 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "00:0:00                       ", "6 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "0:00:00                       ", "6 ", &
        "YES", "\d\d?:\d\d?:\d\d?       ", "00:00:00                      ", "7 ", &
        "YES", "[Hh]ello [Ww]orld\s*[!]?     ", "Hello world !                 ", "12", &
        "YES", "[Hh]ello [Ww]orld\s*[!]?     ", "hello world !                 ", "12", &
        "YES", "[Hh]ello [Ww]orld\s*[!]?     ", "Hello World !                 ", "12", &
        "YES", "[Hh]ello [Ww]orld\s*[!]?     ", "Hello world!                  ", "11", &
        "YES", "[Hh]ello [Ww]orld\s*[!]?     ", "Hello world  !                ", "13", &
        "YES", "[Hh]ello [Ww]orld\s*[!]?     ", "hello World    !              ", "15", &
        "YES", ".?bar                         ", "real_bar                      ", "4 ", &
        "NO ", ".?bar                         ", "real_foo                      ", "0 ", &
        "NO ", "X?Y                           ", "Z                             ", "0 ", &
        "YES", "[a-z]+"//NEWLINE//"break      ", "blahblah"//NEWLINE//"break     ", "14", &
        "YES", "[a-z\s]+"//NEWLINE//"break    ", "bla bla "//NEWLINE//"break     ", "14"],[4,66])

        ! These cases have C-specific characters and need be defined
        ! "YES", "b.\s*\n                      ", "aa\r\nbb\r\ncc\r\n\r\n        ", "4 ", &

    contains

    logical function run_test1(valid,pattern,string,correct_len) result(success)
       logical,      intent(in) :: valid
       character(*), intent(in) :: pattern
       character(*), intent(in) :: string
       integer,      intent(in) :: correct_len

       integer :: idx,length
       type(regex_op) :: re

       print "('regex test: pattern=',a,' string=',a,'....')",trim(pattern),trim(string)

       idx = regex(string, pattern, length)

       success = valid .eqv. (idx>0) .and. (length == correct_len)

       if (.not.success) then
          write(*,*) 'pattern = ',trim(pattern),' string = ',trim(string),' idx = ',idx,' len = ',length
          write(*,*) 'should be found? ',valid,' with length = ',correct_len
          re = parse_pattern(pattern)
          print *, 'pattern breakdown: '
          call re%write()
       endif

    end function run_test1

    subroutine get_test1(itest,valid,pattern,string,length)
        integer, intent(in) :: itest
        logical, intent(out) :: valid
        character(*), intent(out) :: pattern,string
        integer, intent(out) :: length

        character(len(test1data)) :: buffer

        if (.not.(itest>0 .and. itest<=size(test1data,2))) return

        valid   = trim(test1data(1,itest))=='YES'
        pattern = test1data(2,itest)
        string  = test1data(3,itest)
        buffer  = test1data(4,itest); read(buffer,*) length

    end subroutine get_test1

    ! Read in results from test data in tiny-regex-c
    subroutine read_results(fileName)
       character(*), intent(in) :: fileName

       integer :: iunit,ierr,j,i,ntest
       character(len=30) :: openb,isok,pattern,text,chart,length,closeb,test_param(4,100)

       open(newunit=iunit,file=fileName,form='formatted',action='read',iostat=ierr)
       ntest = 0

       do while (.not.is_iostat_end(ierr))

           read(iunit,*,iostat=ierr) openb,isok,pattern,text,chart,length,closeb

           if (ierr==0) then
              ntest = ntest + 1
              test_param(1,ntest) = merge('YES','NO ',trim(isok)=='OK')
              test_param(2,ntest) = pattern
              test_param(3,ntest) = text
              test_param(4,ntest) = length
           end if

       end do

       close(iunit)

       ! Print to output
       open(newunit=iunit,file='testdata.f90',form='formatted',action='write',iostat=ierr)

          write(iunit,1) ntest,len(test_param)
          write(iunit,2) ((adjustl(test_param(i,j)),i=1,4),j=1,ntest-1)
          write(iunit,3) ((adjustl(test_param(i,j)),i=1,4),j=ntest,ntest),4,ntest

       close(iunit)

       1 format(4x,'character(len=*,kind=RCK), parameter :: testdata(4,',i0,') = reshape([ character(len=',i0,') :: &')
       2 format(8x,'"',a3,'", "',a30,'", "',a30,'", "',a2,'", &')
       3 format(8x,'"',a3,'", "',a30,'", "',a30,'", "',a2,'"],[',i0,',',i0,'])')

    end subroutine read_results

end module regex_test_1
