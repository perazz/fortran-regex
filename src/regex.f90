! *************************************************************************************************
!                                    ____  ___________________  __
!                                   / __ \/ ____/ ____/ ____/ |/ /
!                                  / /_/ / __/ / / __/ __/  |   /
!                                 / _, _/ /___/ /_/ / /___ /   |
!                                /_/ |_/_____/\____/_____//_/|_|
!
! MIT License
!
! (C) Federico Perini, 2022
!     A Fortran port of the tiny-regex library.
!
!     https://github.com/kokke/tiny-regex-c
!     Code inspired by Rob Pike's regex code described in:
!     http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
!
! *************************************************************************************************
module regex_module
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: parse_pattern
    public :: regex

    ! Character kind
    integer, parameter, public :: RCK = selected_char_kind("ascii")

    logical, parameter, public :: RE_DOT_MATCHES_NEWLINE = .true. ! Define .false. if you DON'T want '.' to match '\r' + '\n'
    integer, parameter, public :: MAX_REGEXP_OBJECTS = 512        ! Max number of regex symbols in expression.
    integer, parameter, public :: MAX_CHAR_CLASS_LEN = 1024       ! Max length of character-class buffer in.

    ! Turn on verbosity for debugging
    logical, parameter :: DEBUG = .false.

    ! Supported patterns
    integer, parameter :: UNUSED         = 0
    integer, parameter :: DOT            = 1   ! '.'        Dot, matches any character
    integer, parameter :: BEGIN_WITH     = 2   ! '^'        Start anchor, matches beginning of string
    integer, parameter :: END_WITH       = 3   ! '$'        End anchor, matches end of string
    integer, parameter :: QUESTIONMARK   = 4   ! '?'        Question, match zero or one (non-greedy)
    integer, parameter :: STAR           = 5   ! '*'        Asterisk, match zero or more (greedy)
    integer, parameter :: PLUS           = 6   ! '+'        Plus, match one or more (greedy)
    integer, parameter :: ATCHAR         = 7   ! '[a-zA-Z]' Character ranges, the character set of the ranges { a-z | A-Z }
    integer, parameter :: AT_CHAR_CLASS  = 8   ! '[abc]'    Character class, match if one of {'a', 'b', 'c'}
    integer, parameter :: INV_CHAR_CLASS = 9   ! '[^abc]'   Inverted class, match if NOT one of {'a', 'b', 'c'} -- NOTE: feature is currently broken!
    integer, parameter :: DIGIT          = 10  ! '\d'       Digits, [0-9]
    integer, parameter :: NOT_DIGIT      = 11  ! '\D'       Non-digits
    integer, parameter :: ALPHA          = 12  ! '\w'       Alphanumeric, [a-zA-Z0-9_]
    integer, parameter :: NOT_ALPHA      = 13  ! '\W'       Non-alphanumeric
    integer, parameter :: WHITESPACE     = 14  ! '\s'       Whitespace, \t \f \r \n \v and spaces
    integer, parameter :: NOT_WHITESPACE = 15  ! '\S'       Non-whitespace

    character(kind=RCK,len=*), parameter :: types(*) = [ character(len=14) :: "UNUSED", "DOT", "BEGIN", "END", "QUESTIONMARK", &
        "STAR", "PLUS", "CHAR", "CHAR_CLASS", "INV_CHAR_CLASS", "DIGIT", "NOT_DIGIT", "ALPHA", "NOT_ALPHA", &
        "WHITESPACE", "NOT_WHITESPACE", "BRANCH" ]

    ! Characters
    character(kind=RCK,len=*), parameter :: lowercase="abcdefghijklmnopqrstuvwxyz"
    character(kind=RCK,len=*), parameter :: uppercase="ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    character(kind=RCK), parameter, public :: UNDERSCORE = "_"
    character(kind=RCK), parameter, public :: SPACE      = " "
    character(kind=RCK), parameter, public :: DASH       = "-"
    character(kind=RCK), parameter, public :: CNULL      = achar( 0,kind=RCK)  ! \0 or null character
    character(kind=RCK), parameter, public :: NEWLINE    = achar(10,kind=RCK)  ! \n or line feed
    character(kind=RCK), parameter, public :: BACKSPCE   = achar( 8,kind=RCK)  ! \b or backspace character


    ! Regex pattern element
    type, public :: regex_pattern

        integer :: type = UNUSED

        ! Single or multi-character pattern
        character(kind=RCK,len=MAX_CHAR_CLASS_LEN) :: ccl = repeat(' ',MAX_CHAR_CLASS_LEN)
        contains

          procedure :: print => print_pattern
          procedure :: destroy => pat_destroy
          procedure :: match => pat_match

    end type regex_pattern

    type, public :: regex_op

        integer :: n = 0

        type(regex_pattern), dimension(MAX_REGEXP_OBJECTS) :: pattern

        contains

           procedure :: new     => new_from_pattern
           procedure :: write   => write_pattern
           procedure :: nrules
           procedure :: destroy
           final     :: finalize

    end type regex_op

    ! Public interface
    interface regex
        module procedure re_match
        module procedure re_match_nolength
        module procedure re_matchp
        module procedure re_matchp_nolength
    end interface regex

    contains

    ! Clean up a pattern
    elemental subroutine pat_destroy(this)
       class(regex_pattern), intent(inout) :: this
       this%type = UNUSED
       this%ccl  = repeat(' ',MAX_CHAR_CLASS_LEN)
    end subroutine pat_destroy

    ! Number of rules in the current pattern
    elemental integer function nrules(this)
       class(regex_op), intent(in) :: this
       integer :: i
       nrules = 0
       do i=1,MAX_REGEXP_OBJECTS
          if (this%pattern(i)%type==UNUSED) return
          nrules = nrules + 1
       end do
    end function nrules

    subroutine write_pattern(this,iunit)
        class(regex_op), intent(in) :: this
        integer, optional, intent(in) :: iunit

        integer :: i,u

        if (present(iunit)) then
            u = iunit
        else
            u = output_unit
        end if

        do i=1,this%nrules()
           write(u,'(a)') this%pattern(i)%print()
        end do

    end subroutine write_pattern

    elemental subroutine destroy(this)
        class(regex_op), intent(inout) :: this
        integer :: i,ierr
        do i=1,MAX_REGEXP_OBJECTS
            this%pattern(i)%type = UNUSED
            this%pattern(i)%ccl = ''
        end do
    end subroutine destroy

    subroutine finalize(this)
        type(regex_op), intent(inout) :: this
        integer :: i,ierr
        do i=1,MAX_REGEXP_OBJECTS
            this%pattern(i)%type = UNUSED
            this%pattern(i)%ccl = ''
        end do
    end subroutine finalize

    ! Check that a character matches a dot ("any character") pattern
    elemental logical function matchdot(c)
       character(kind=RCK), intent(in) :: c
       if (RE_DOT_MATCHES_NEWLINE) then
          matchdot = .true.
       else
          matchdot = c/=NEWLINE .and. c/=BACKSPCE
       end if
    end function matchdot

    elemental logical function ismetachar(c)
       character(kind=RCK), intent(in) :: c
       ismetachar = index("sSwWdD",c)>0
    end function ismetachar

    pure logical function matchmetachar(c, str)
       character(kind=RCK), intent(in) :: c
       character(kind=RCK,len=*), intent(in) :: str

       select case (str(1:1))
          case ('d');   matchmetachar =      isdigit(c)
          case ('D');   matchmetachar = .not.isdigit(c)
          case ('w');   matchmetachar =      isalphanum(c)
          case ('W');   matchmetachar = .not.isalphanum(c)
          case ('s');   matchmetachar =      isspace(c)
          case ('S');   matchmetachar = .not.isspace(c)
          case default; matchmetachar = c==str(1:1)
       end select
    end function matchmetachar

    elemental logical function isdigit(c)
       character(kind=RCK), intent(in) :: c
       isdigit = index("1234567890",c)>0
    end function isdigit

    elemental logical function isalpha(c)
       character(kind=RCK), intent(in) :: c
       isalpha = index(lowercase,c)>0 .or. index(uppercase,c)>0
    end function isalpha

    elemental logical function isalphanum(c)
       character(kind=RCK), intent(in) :: c
       isalphanum = isalpha(c) .or. isdigit(c) .or. c==UNDERSCORE
    end function isalphanum

    elemental logical function isspace(c)
       character(kind=RCK), intent(in) :: c
       isspace = c==SPACE
    end function isspace

    ! Match range of the tye 0-9  or 5-7 etc.
    elemental logical function matchrange(c,str)
       character(kind=RCK), intent(in) :: c
       character(kind=RCK,len=*), intent(in) :: str ! the range pattern

       matchrange = len(str)>=3 &
                    .and. c /= DASH &
                    .and. str(1:1) /= DASH &
                    .and. str(2:2) == DASH &
                    .and. iachar(c)>=iachar(str(1:1)) &
                    .and. iachar(c)<=iachar(str(3:3))    ! Range (number/letters) is in increasing order

    end function matchrange

    logical function matchcharclass(c,str) result(match)
       character(kind=RCK), intent(in) :: c         ! The current character
       character(kind=RCK,len=*), intent(in) :: str ! The charclass contents

       integer :: i

       match = .false.
       i = 0

       ! All characters in the charclass contents
       loop: do while (i<len_trim(str))

          i = i+1

          ! We're in a range: must check this further
          if (matchrange(c,str(i:))) then
            match = .true.
            return

          ! Escaped character? look what's next
          elseif (str(i:i) == '\') then

             i = i+1

             ! Valid escaped sequence

             if (matchmetachar(c,str(i:))) then
                match = .true.
                return
             elseif (c==str(i:i) .and. (.not.ismetachar(c))) then
                match = .true.
                return
             endif

          elseif (c==str(i:i)) then

             ! Character match
             if (c==DASH) then
                ! If this is a range, the character must be in this range, that we evaluate with the ASCII collating sequence
                match = i<=0 .or. i+1>len_trim(str)
             else
                match = .true.
             end if
             return
          end if

       end do loop

       if (DEBUG) print *, 'charclass: no match on i=',i,' str=',trim(str),' c=',c

    end function matchcharclass

    logical function matchquestion(p, pattern, text, matchlength)
       type(regex_pattern), intent(in) :: p, pattern(:)
       character(len=*,kind=RCK), intent(in) :: text
       integer, intent(inout) :: matchlength

       matchquestion = .false.

       if (p%type == UNUSED) then
          matchquestion = .true.
          return
       elseif (matchpattern(pattern, text, matchlength)) then
          matchquestion = .true.
          return
       elseif (len(text)>0) then
          if (pat_match(p,text) .and. len(text)>1) then
             if (matchpattern(pattern,text(2:),matchlength)) then
                matchlength  = matchlength+1
                matchquestion = .true.
                return
             endif
          end if
       end if

    end function matchquestion

    logical function matchstar(p, pattern, text, it0, matchlength)
       type(regex_pattern), intent(in) :: p, pattern(:)
       character(len=*,kind=RCK), intent(in) :: text
       integer, intent(in)    :: it0 ! starting point
       integer, intent(inout) :: matchlength

       integer :: prelen,it

       if (DEBUG) print *, 'match star, length=',matchlength,' it0=',it0,' lenm=',len(text)

       if (len(text)<=0) then
          matchstar = .false.
          return
       end if

       ! Save input variables
       prelen   = matchlength
       it = it0

       do while (it>0 .and. it<=len(text))
          if (.not.pat_match(p, text(it:))) exit
          it          = it+1
          matchlength = matchlength+1
       end do

       do while (it>=it0)
         matchstar = matchpattern(pattern, text(it:), matchlength)
         it          = it-1
         if (matchstar) return
         matchlength = matchlength-1

       end do

       matchlength = prelen
       matchstar   = .false.

    end function matchstar

    logical function matchplus(p, pattern, text, it0, matchlength)
       type(regex_pattern), intent(in) :: p, pattern(:)
       character(len=*,kind=RCK), intent(in) :: text
       integer, intent(in) :: it0
       integer, intent(inout) :: matchlength

       integer :: it

       if (DEBUG) print *, 'matching PLUS pattern'

       it = it0
       do while (it>0 .and. it<=len(text))
          if (.not. pat_match(p, text(it:))) exit
          it = it+1
          matchlength = matchlength+1
       end do

       do while (it>it0)
          matchplus = matchpattern(pattern, text(it:), matchlength)
          it = it-1
          if (matchplus) return
          matchlength = matchlength-1
       end do

       matchplus = .false.

    end function matchplus

    ! Find matches of the given pattern in the string
    integer function re_match(text, pattern, length) result(index)
       character(*,kind=RCK), intent(in) :: pattern
       character(*,kind=RCK), intent(in) :: text
       integer, intent(out) :: length
       type (regex_op) :: command

       command = parse_pattern(pattern)
       index = re_matchp(text,command,length)

    end function re_match

    ! Find matches of the given pattern in the string
    integer function re_match_nolength(text, pattern) result(index)
       character(*,kind=RCK), intent(in) :: pattern
       character(*,kind=RCK), intent(in) :: text

       type (regex_op) :: command
       integer :: length

       command = parse_pattern(pattern)
       index = re_matchp(text,command,length)

    end function re_match_nolength

    type(regex_op) function parse_pattern(pattern) result(this)
       character(*,kind=RCK), intent(in) :: pattern

       call new_from_pattern(this,pattern)

    end function parse_pattern

    subroutine new_from_pattern(this,pattern)
       class(regex_op), intent(inout) :: this
       character(*,kind=RCK), intent(in) :: pattern

       ! Local variables
       character(len=MAX_CHAR_CLASS_LEN,kind=RCK) :: ccl_buf ! size of buffer for chars in all char-classes in the expression. */
       integer :: loc,i,j,lenp
       character(kind=RCK) :: c

       ! Initialize class
       call this%destroy()

       if (DEBUG) print "('[regex] parsing pattern: <',a,'>')", trim(pattern)

       i = 1 ! index in pattern
       j = 1 ! index in re-compiled
       lenp = len_trim(pattern)

       ! Move along the pattern string
       to_the_moon: do while (i<=lenp)

         c = pattern(i:i)
         if (DEBUG) print "('[regex] at location ',i0,': <',a,'>')", i, c

         select case (c)

            ! Meta-characters are single-character patterns
            case ('^'); this%pattern(j) = regex_pattern(BEGIN_WITH,c)
            case ('$'); this%pattern(j) = regex_pattern(END_WITH,c)
            case ('.'); this%pattern(j) = regex_pattern(DOT,c)
            case ('*'); this%pattern(j) = regex_pattern(STAR,c)
            case ('+'); this%pattern(j) = regex_pattern(PLUS,c)
            case ('?'); this%pattern(j) = regex_pattern(QUESTIONMARK,c)

            ! Escaped character-classes (\s, \w, ...)
            case ('\');

                ! Parse an escaped character class
                if (i<lenp) then

                    ! There's something next: check it
                    i = i+1;

                    select case (pattern(i:i))
                       case ('d'); this%pattern(j) = regex_pattern(DIGIT,'d')
                       case ('D'); this%pattern(j) = regex_pattern(NOT_DIGIT,'D')
                       case ('w'); this%pattern(j) = regex_pattern(ALPHA,'w')
                       case ('W'); this%pattern(j) = regex_pattern(NOT_ALPHA,'W')
                       case ('s'); this%pattern(j) = regex_pattern(WHITESPACE,'s')
                       case ('S'); this%pattern(j) = regex_pattern(NOT_WHITESPACE,'S')
                       case default;
                            ! Escaped character: "." or "$"
                            this%pattern(j) = regex_pattern(ATCHAR,pattern(i:i))
                    end select

                else

                    ! This is the first character of a sequence *and* the end of rht pattern. store as CHAR
                    this%pattern(j) = regex_pattern(ATCHAR,c)

                endif

            ! Character class
            case ('[')

                loc = 1

                ! First, check if this class is negated ("^")
                if (pattern(i+1:i+1)=='^') then
                    this%pattern(j)%type = INV_CHAR_CLASS

                    i = i+1 ! Increment i to avoid including "^" in the char-buffer

                    ! Check this is not an incomplete pattern
                    if (i>=lenp) then
                        stop 'incomplete pattern'
                        return
                    end if

                else
                    this%pattern(j)%type = AT_CHAR_CLASS
                end if

                ! Remove any escape characters
                loc = index(pattern(i+1:),']')
                if (loc>0) then
                    ccl_buf = pattern(i+1:i+loc-1)
                    i = i+loc
                    if (DEBUG) print "('[regex] at end of multi-character pattern: ',a)", trim(ccl_buf)
                else
                    stop 'incomplete [] pattern'
                end if

                ! If there is any escape character(s), just check that the next is nonempty
                loc = index(ccl_buf,'\')
                if (loc>0) then
                    if (loc>=len(ccl_buf)) then
                        stop 'incomplete escaped character inside [] pattern'
                    end if
                    if (ccl_buf(loc+1:loc+1)==SPACE) then
                        stop 'empty escaped character inside [] pattern'
                    end if
                end if

                ! Terminate string
                this%pattern(j)%ccl = trim(ccl_buf)

         case default

             ! Single character
             this%pattern(j) = regex_pattern(ATCHAR,c)

         end select

         if (DEBUG) print "('[regex] added pattern ',i0,': ',a)",j,this%pattern(j)%print()

         ! A pattern was added: move to next
         i = i+1
         j = j+1
         if (j>MAX_REGEXP_OBJECTS) stop 'max regexp reached!'

       end do to_the_moon

       ! Save number of patterns
       this%n = j-1

    end subroutine new_from_pattern

    function print_pattern(pattern) result(msg)
        class(regex_pattern), intent(in) :: pattern
        character(:,kind=RCK), allocatable :: msg

        character(len=MAX_CHAR_CLASS_LEN,kind=RCK) :: buffer
        integer :: lt

        write(buffer,1) trim(types(pattern%type+1)),trim(pattern%ccl)

        lt = len_trim(buffer)
        allocate(character(len=lt,kind=RCK) :: msg)
        if (lt>0) msg(1:lt) = buffer(1:lt)

        1 format('type=',a,:,1x,'char=',a)

    end function print_pattern

    ! Match a single pattern at the g
    logical function pat_match(p, c) result(match)
       class(regex_pattern), intent(in) :: p
       character(kind=RCK), intent(in) :: c

       select case (p%type)
          case (DOT);            match = matchdot(c)
          case (AT_CHAR_CLASS);  match = matchcharclass(c,p%ccl)
          case (INV_CHAR_CLASS); match = .not.matchcharclass(c,p%ccl)
          case (DIGIT);          match = isdigit(c)
          case (NOT_DIGIT);      match = .not.isdigit(c)
          case (ALPHA);          match = isalphanum(c)
          case (NOT_ALPHA);      match = .not.isalphanum(c)
          case (WHITESPACE);     match = isspace(c)
          case (NOT_WHITESPACE); match = .not.isspace(c)
          case default;          match = c==p%ccl(1:1)
       end select

       if (DEBUG) print "('[regex] current pattern=',a,' at char=',a,' match? ',l1)", p%print(),c,match

    end function pat_match

    integer function re_matchp_nolength(text, pattern) result(index)
       type(regex_op), intent(in) :: pattern
       character(len=*,kind=RCK), intent(in) :: text
       integer :: matchlength
       index = re_matchp(text, pattern, matchlength)
    end function re_matchp_nolength


    integer function re_matchp(text, pattern, matchlength) result(index)
       type(regex_op), intent(in) :: pattern
       character(len=*,kind=RCK), intent(in) :: text
       integer, intent(out) :: matchlength

       matchlength = 0

       if (pattern%n>0) then

          if (pattern%pattern(1)%type == BEGIN_WITH) then

             ! String must begin with this pattern
             index = merge(1,0,matchpattern(pattern%pattern(2:), text, matchlength))

          else

             do index=1,len(text)
                if (matchpattern(pattern%pattern,text(index:),matchlength)) return
             end do

             index = 0

          end if

       else

          index = 0

       end if


    end function re_matchp







   ! Iterative matching
   logical function matchpattern(pattern, text, matchlength) result(match)
      class(regex_pattern), intent(in) :: pattern(:)
      character(kind=RCK,len=*), intent(in) :: text
      integer, intent(inout) :: matchlength

      integer :: pre,ip,it

      pre = matchlength
      ip  = 1
      it  = 1

      iterate: do while (ip<=size(pattern))

         if (pattern(ip)%type == UNUSED .or. pattern(ip+1)%type == QUESTIONMARK) then

            match = matchquestion(pattern(ip),pattern(ip+2:),text(it:),matchlength)
            return

         elseif (pattern(ip+1)%type == STAR) then

            match = matchstar(pattern(ip),pattern(ip+2:), text, it, matchlength)
            return

         elseif (pattern(ip+1)%type == PLUS) then

            match = matchplus(pattern(ip),pattern(ip+2:), text, it, matchlength)
            return

         elseif (pattern(ip)%type == END_WITH .and. pattern(ip+1)%type == UNUSED) then


            match = len(text(it:))<=1
            return

         end if

         matchlength = matchlength+1

         if (it>len(text)) exit iterate

         if (DEBUG) print "('[regex] matching ',i0,'-th pattern on chunk <',i0,':',i0,'>')", ip,it,len(text)
         if (.not. pat_match(pattern(ip), text(it:it))) exit iterate
         ip = ip+1
         it = it+1

      end do iterate

      matchlength = pre
      match = .false.
      return

   end function matchpattern




end module regex_module
