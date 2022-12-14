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
module regex
    implicit none
    private

    public :: parse_pattern
    !public :: re_matchp
    !public :: re_match

    ! Character kind
    integer, parameter, public :: RCK = selected_char_kind("ascii")

    logical, parameter, public :: RE_DOT_MATCHES_NEWLINE = .true. ! Define .false. if you DON'T want '.' to match '\r' + '\n'
    integer, parameter, public :: MAX_REGEXP_OBJECTS = 512        ! Max number of regex symbols in expression.
    integer, parameter, public :: MAX_CHAR_CLASS_LEN = 1024       ! Max length of character-class buffer in.
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

    character(kind=RCK), parameter :: UNDERSCORE = "_"
    character(kind=RCK), parameter :: SPACE      = " "
    character(kind=RCK), parameter :: DASH       = "-"
    character(kind=RCK), parameter :: BACKSLASH0 = achar(0,kind=RCK)  ! \0 or null character
    character(kind=RCK), parameter :: NEWLINE    = achar(10,kind=RCK) ! \n or line feed
    character(kind=RCK), parameter :: BACKSPCE   = achar(8,kind=RCK)  ! \b or backspace character


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

           procedure :: parse => parse_pattern
           procedure :: write => write_pattern
           procedure :: nrules
           procedure :: destroy
           final :: finalize

    end type regex_op

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
        integer, intent(in) :: iunit

        integer :: i

        do i=1,this%nrules()
           write(iunit,'(a)') this%pattern(i)%print()
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
       ismetachar = index(c,"sSwWdD")>0
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
       isdigit = index(c,"01234567890")>0
    end function isdigit

    elemental logical function isalpha(c)
       character(kind=RCK), intent(in) :: c
       isalpha = index(c,lowercase)>0 .or. index(c,uppercase)>0
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
       character(kind=RCK,len=*), intent(in) :: str

       matchrange = len(str)>=3 .and. &
                    c /= DASH &
                    .and. str(1:1) /= DASH &
                    .and. str(2:2) == DASH &
                    .and. ichar(c)>=ichar(str(1:1)) &
                    .and. ichar(c)<=ichar(str(3:3))    ! Range (number/letters) is in increasing order

    end function matchrange

    elemental logical function matchcharclass(c,str)
       character(kind=RCK), intent(in) :: c
       character(kind=RCK,len=*), intent(in) :: str

       integer :: i

       matchcharclass = .false.
       i = 0

       loop: do

          i = i+1

          if (matchrange(c,str(i:))) then
             matchcharclass = .true.
             return
          elseif (str(i:i+1) == BACKSLASH0) then
             ! Escape-char: increment str-ptr and match on next char
             i = i+1

             if (matchmetachar(c,str(i:))) then
                matchcharclass = .true.
                return
             elseif (c==str(i:i) .and. .not.ismetachar(c)) then
                matchcharclass = .true.
                return
             end if
          elseif (c==str(i:i)) then
             if (c==DASH) then
                matchcharclass = str(i-1:i-1)==BACKSLASH0 .or. str(i+1:i+1)==BACKSLASH0
             else
                matchcharclass = .true.
             end if
             return
          end if

          if (i==len(str)) exit loop

       end do loop

    end function matchcharclass

    ! Find matches of the given pattern in the string
    integer function re_match(pattern, text, length)
       character(*,kind=RCK), intent(in) :: pattern
       character(*,kind=RCK), intent(in) :: text
       integer, intent(out) :: length

       !re_match = re_matchp(parse_pattern(pattern),text,length)
       stop 're_match not impl'

    end function re_match

    subroutine parse_pattern(this, pattern)
       class(regex_op), intent(inout) :: this
       character(*,kind=RCK), intent(in) :: pattern

       ! Local variables
       character(len=MAX_CHAR_CLASS_LEN,kind=RCK) :: ccl_buf ! size of buffer for chars in all char-classes in the expression. */
       integer :: loc,i,j,lenp
       character(kind=RCK) :: c

       ! Initialize class
       call this%destroy()

       if (DEBUG) print *, "[regex] parsing pattern: <"//pattern//">"

       i = 1 ! index in pattern
       j = 1 ! index in re-compiled
       lenp = len_trim(pattern)

       ! Move along the pattern string
       to_the_moon: do while (i<=lenp)

         c = pattern(i:i)
         if (DEBUG) print *, "[regex] at location ",i,': <',c,'>'

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

                ! First, check if this class is negated ("^")
                if (pattern(i+1:i+1)=='^') then
                    this%pattern(j)%type = INV_CHAR_CLASS

                    i = i+1 ! Increment i to avoid including "^" in the char-buffer

                    ! Check this is not an incomplete pattern
                    if (pattern(i+1:i+1)==BACKSLASH0) then
                        stop 'incomplete pattern'
                        return
                    end if

                else
                    this%pattern(j)%type = AT_CHAR_CLASS
                end if

                ! Copy characters inside [..] to buffer */
                loc = index(pattern(i+1:),']')

                if (loc>0) then
                    ccl_buf = pattern(i+1:i+loc-1)
                    i = i+loc+1
                else
                    stop 'incomplete [] pattern'
                end if

                ! the only escaped character between brackets is \\
                ! if present, replace double backslash with a single one
                loc = index(ccl_buf,'\\')
                if (loc>0) ccl_buf = ccl_buf(:loc)//ccl_buf(loc+2:)

                ! Terminate string
                this%pattern(j)%ccl = trim(ccl_buf)

         case default

             ! Single character
             this%pattern(j) = regex_pattern(ATCHAR,c)

         end select

         if (DEBUG) print *, "[regex] added pattern ",j,': ',this%pattern(j)%print()

         ! A pattern was added: move to next
         i = i+1
         j = j+1
         if (j>MAX_REGEXP_OBJECTS) stop 'max regexp reached!'

       end do to_the_moon

       ! Save number of patterns
       this%n = j-1

    end subroutine parse_pattern

    function print_pattern(pattern) result(msg)
        class(regex_pattern), intent(in) :: pattern
        character(:,kind=RCK), allocatable :: msg

        character(len=MAX_CHAR_CLASS_LEN,kind=RCK) :: buffer
        integer :: lt

        write(buffer,1) types(pattern%type+1),trim(pattern%ccl)

        lt = len_trim(buffer)
        allocate(character(len=lt,kind=RCK) :: msg)
        if (lt>0) msg(1:lt) = buffer(1:lt)

        1 format('type=',a,:,1x,'char=',a)

    end function print_pattern

    ! Match a single pattern at the g
    elemental logical function pat_match(p, c) result(match)
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

    end function pat_match


    integer function re_matchp(pattern, text, matchlength) result(index)
       type(regex_op) :: pattern
       character(len=*,kind=RCK), intent(in) :: text
       integer, intent(out) :: matchlength

       matchlength = 0

       if (pattern%n>0) then

          if (pattern%pattern(1)%type == BEGIN_WITH) then

             ! String must begin with this pattern
             index = merge(1,0,matchpattern([pattern%pattern(2)], text, matchlength))

          else

             do index=1,len(text)
                if (matchpattern(pattern%pattern,text,matchlength)) return
             end do

             index = 0

          end if

       else

          index = 0

       end if


    end function re_matchp



!
!static int matchstar(regex_pattern p, regex_pattern* pattern, const char* text, int* matchlength)
!{
!  int prelen = *matchlength;
!  const char* prepoint = text;
!  while ((text[0] != '\0') && pat_match(p, *text))
!  {
!    text++;
!    (*matchlength)++;
!  }
!  while (text >= prepoint)
!  {
!    if (matchpattern(pattern, text--, matchlength))
!      return 1;
!    (*matchlength)--;
!  }
!
!  *matchlength = prelen;
!  return 0;
!}
!
!static int matchplus(regex_pattern p, regex_pattern* pattern, const char* text, int* matchlength)
!{
!  const char* prepoint = text;
!  while ((text[0] != '\0') && pat_match(p, *text))
!  {
!    text++;
!    (*matchlength)++;
!  }
!  while (text > prepoint)
!  {
!    if (matchpattern(pattern, text--, matchlength))
!      return 1;
!    (*matchlength)--;
!  }
!
!  return 0;
!}
!
!static int matchquestion(regex_pattern p, regex_pattern* pattern, const char* text, int* matchlength)
!{
!  if (p.type == UNUSED)
!    return 1;
!  if (matchpattern(pattern, text, matchlength))
!      return 1;
!  if (*text && pat_match(p, *text++))
!  {
!    if (matchpattern(pattern, text, matchlength))
!    {
!      (*matchlength)++;
!      return 1;
!    }
!  }
!  return 0;
!}
!
!

   ! Iterative matching
   logical function matchpattern(pattern, text, matchlength) result(match)
      class(regex_pattern), intent(in) :: pattern(:)
      character(kind=RCK,len=*), intent(in) :: text
      integer, intent(in) :: matchlength

      integer :: pre

      pre = matchlength

!
!      iterate: do
!
!
!  do
!  {
!    if ((pattern[0].type == UNUSED) || (pattern[1].type == QUESTIONMARK))
!    {
!      return matchquestion(pattern[0], &pattern[2], text, matchlength);
!    }
!    else if (pattern[1].type == STAR)
!    {
!      return matchstar(pattern[0], &pattern[2], text, matchlength);
!    }
!    else if (pattern[1].type == PLUS)
!    {
!      return matchplus(pattern[0], &pattern[2], text, matchlength);
!    }
!    else if ((pattern[0].type == END) && pattern[1].type == UNUSED)
!    {
!      return (text[0] == '\0');
!    }
!/*  Branching is not working properly
!    else if (pattern[1].type == BRANCH)
!    {
!      return (matchpattern(pattern, text) || matchpattern(&pattern[2], text));
!    }
!*/
!  (*matchlength)++;
!  }
!  while ((text[0] != '\0') && pat_match(*pattern++, *text++));
!
!   *matchlength = pre;
!
!   ierr = 0
!
!   end function matchpattern
!
!static int matchpattern(regex_pattern* pattern, const char* text, int* matchlength)
!{
!  int pre = *matchlength;
!
!}
!
!#endif
!
!
   end function matchpattern




end module regex
