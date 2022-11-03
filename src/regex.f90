! *************************************************************************************************
!                                    ____  ___________________  __
!                                   / __ \/ ____/ ____/ ____/ |/ /
!                                  / /_/ / __/ / / __/ __/  |   /
!                                 / _, _/ /___/ /_/ / /___ /   |
!                                /_/ |_/_____/\____/_____//_/|_|
!
! (C) Federico Perini, 2022- A Fortran port of the mini-regex library.
!     Code inspired to tiny-regex-c from
!     Mini regex-module inspired by Rob Pike's regex code described in:
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
    integer, parameter :: ATBEGIN        = 2   ! '^'        Start anchor, matches beginning of string
    integer, parameter :: ATEND          = 3   ! '$'        End anchor, matches end of string
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
        integer :: type ! CHAR, star, ...
        character(kind=RCK,len=:), allocatable :: ccl ! Characters in class
        contains

          procedure :: print => print_pattern

    end type regex_pattern

    type, public :: regex_op
        type(regex_pattern), dimension(MAX_REGEXP_OBJECTS) :: pattern

        contains

           procedure :: parse => parse_pattern
           procedure :: write => write_pattern
           procedure :: nrules
           procedure :: destroy
           final :: finalize

    end type regex_op



! *
! *
! * Supports:
! * ---------

! *
! *
! */


!/* Compile regex string pattern to a regex_pattern-array. */
!re_t re_compile(const char* pattern);
!
!
!/* Find matches of the compiled pattern inside text. */
!int re_matchp(re_t pattern, const char* text, int* matchlength);
!
!
!/* Find matches of the txt pattern inside text (will compile automatically first). */
!int re_match(const char* pattern, const char* text, int* matchlength);

    contains

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
            deallocate(this%pattern(i)%ccl,stat=ierr)
        end do
    end subroutine destroy

    subroutine finalize(this)
        type(regex_op), intent(inout) :: this
        integer :: i,ierr
        do i=1,MAX_REGEXP_OBJECTS
            this%pattern(i)%type = UNUSED
            deallocate(this%pattern(i)%ccl,stat=ierr)
        end do
    end subroutine finalize

    elemental logical function matchdot(c)
       character(kind=RCK), intent(in) :: c
       if (RE_DOT_MATCHES_NEWLINE) then
          !  (void)c;
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
                    c        /= DASH &
                    .and. str(1:1) /= BACKSLASH0 &
                    .and. str(1:1) /= DASH &
                    .and. str(2:2) == DASH &
                    .and. str(3:3) /= BACKSLASH0 &
                    .and. c>=str(1:1) .and. c<=str(3:3) ! wtf?

    end function matchrange

    elemental logical function ischarclass(c,str)
       character(kind=RCK), intent(in) :: c
       character(kind=RCK,len=*), intent(in) :: str

       integer :: i

       ischarclass = .false.
       i = 0

       loop: do

          i = i+1

          if (matchrange(c,str(i:))) then
             ischarclass = .true.
             return
          elseif (str(i:i+1) == BACKSLASH0) then
             ! Escape-char: increment str-ptr and match on next char
             i = i+1

             if (matchmetachar(c,str(i:))) then
                ischarclass = .true.
                return
             elseif (c==str(i:i) .and. .not.ismetachar(c)) then
                ischarclass = .true.
                return
             end if
          elseif (c==str(i:i)) then
             if (c==DASH) then
                ischarclass = str(i-1:i-1)==BACKSLASH0 .or. str(i+1:i+1)==BACKSLASH0
             else
                ischarclass = .true.
             end if
             return
          end if

          if (i==len(str)) exit loop

       end do loop

    end function ischarclass

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
       integer :: ccl_bufidx,buf_begin,i,j,lenp
       character(kind=RCK) :: c

       ! Initialize class
       call this%destroy()

       if (DEBUG) print *, "[regex] parsing pattern: <"//pattern//">"

       i = 1 ! index in pattern
       j = 1 ! index in re-compiled
       ccl_bufidx = 1
       lenp = len_trim(pattern)

       do while (i<=lenp)

         c = pattern(i:i)
         if (DEBUG) print *, "[regex] at location ",i,': <',c,'>'

         select case (c)

            ! Meta-characters are single-character patterns
            case ('^'); this%pattern(j) = regex_pattern(ATBEGIN,c)
            case ('$'); this%pattern(j) = regex_pattern(ATEND,c)
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

                buf_begin = ccl_bufidx ! Remember where the char-buffer starts

                ! Copy characters inside [..] to buffer */
                copy_buf: do while (i<lenp)
                    i = i+1

                    ! Until the end of this pattern
                    if (pattern(i:i)==']') exit copy_buf

                    if (pattern(i:i)==BACKSLASH0) then
                        if (ccl_bufidx>=MAX_CHAR_CLASS_LEN) stop 'exceeded internal buffer!'
                    end if

                    ! Incomplete pattern, missing non-zero char after '\\'
                    if (pattern(i+1:i+1)==achar(0)) then
                        print *, 'pattern(i)=',pattern(i:i),' i+1=',pattern(i+1:i+1),' len=',lenp,' i=',i
                        stop 'incomplete pattern'
                        !return 0
                    end if

                    ccl_buf(ccl_bufidx:ccl_bufidx) = pattern(i:i)
                    ccl_bufidx = ccl_bufidx+1
                    i          = i+1

                end do copy_buf

!        if (ccl_bufidx >= MAX_CHAR_CLASS_LEN)
!        {
!            /* Catches cases such as [00000000000000000000000000000000000000][ */
!            //fputs("exceeded internal buffer!\n", stderr);
!            return 0;
!        }

                ! Terminate string
                ccl_buf(ccl_bufidx+1:MAX_CHAR_CLASS_LEN) = ' '
                this%pattern(j)%ccl = trim(ccl_buf)

         end select

         if (DEBUG) print *, "[regex] added pattern ",j,': ',this%pattern(j)%print()

         ! A pattern was added: move to next
         i = i+1
         j = j+1

       end do

       ! Flag all unused patterns
       this%pattern(j:MAX_REGEXP_OBJECTS)%type = UNUSED

    end subroutine parse_pattern

    function print_pattern(pattern) result(msg)
        class(regex_pattern), intent(in) :: pattern
        character(:,kind=RCK), allocatable :: msg

        character(len=MAX_CHAR_CLASS_LEN,kind=RCK) :: buffer
        integer :: lt

        write(buffer,1) types(pattern%type+1),pattern%ccl

        lt = len_trim(buffer)
        allocate(character(len=lt,kind=RCK) :: msg)
        if (lt>0) msg(1:lt) = buffer(1:lt)

        1 format('type=',a,:,1x,'char=',a)

    end function print_pattern


!}
!
!void re_print(regex_pattern* pattern)
!{

!
!  int i;
!  int j;
!  char c;
!  for (i = 0; i < MAX_REGEXP_OBJECTS; ++i)
!  {
!    if (pattern[i].type == UNUSED)
!    {
!      break;
!    }
!

!  }
!}
!
!
!


!
!int re_matchp(re_t pattern, const char* text, int* matchlength)
!{
!  *matchlength = 0;
!  if (pattern != 0)
!  {
!    if (pattern[0].type == BEGIN)
!    {
!      return ((matchpattern(&pattern[1], text, matchlength)) ? 0 : -1);
!    }
!    else
!    {
!      int idx = -1;
!
!      do
!      {
!        idx += 1;
!
!        if (matchpattern(pattern, text, matchlength))
!        {
!          if (text[0] == '\0')
!            return -1;
!
!          return idx;
!        }
!      }
!      while (*text++ != '\0');
!    }
!  }
!  return -1;
!}
!

!
!static int matchone(regex_pattern p, char c)
!{
!  switch (p.type)
!  {
!    case DOT:            return matchdot(c);
!    case CHAR_CLASS:     return  matchcharclass(c, (const char*)p.u.ccl);
!    case INV_CHAR_CLASS: return !matchcharclass(c, (const char*)p.u.ccl);
!    case DIGIT:          return  isdigit(c);
!    case NOT_DIGIT:      return !isdigit(c);
!    case ALPHA:          return  isalphanum(c);
!    case NOT_ALPHA:      return !isalphanum(c);
!    case WHITESPACE:     return  isspacce(c);
!    case NOT_WHITESPACE: return !isspacce(c);
!    default:             return  (p.u.ch == c);
!  }
!}
!
!static int matchstar(regex_pattern p, regex_pattern* pattern, const char* text, int* matchlength)
!{
!  int prelen = *matchlength;
!  const char* prepoint = text;
!  while ((text[0] != '\0') && matchone(p, *text))
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
!  while ((text[0] != '\0') && matchone(p, *text))
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
!  if (*text && matchone(p, *text++))
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
   integer function matchpattern(pattern, text, matchlength) result(ierr)
      class(regex_pattern), intent(inout) :: pattern
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
!  while ((text[0] != '\0') && matchone(*pattern++, *text++));
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
