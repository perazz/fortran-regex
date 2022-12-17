### fortran-regex

Fortran-regex is a Modern Fortran port of the [tiny-regex-c](https://github.com/kokke/tiny-regex-c) library for regular expressions. It is based on the original C implementation, but the API is modelled in Fortran style, which is similar to the intrinsic `index` function. 

### API

The main API is modelled around Fortran's `index` intrinsic function (which performs a simple search for a substring within a string): 

```fortran
   ! Simple regex
   result = REGEX(string, pattern)
   
   ! Regex with output matched pattern length
   result = REGEX(string, pattern, length)
```

### Object-oriented interface

One can also parse a regex pattern into a `type(regex_op)` structure, and use that instead of a string pattern. I have no idea why this should be useful, but at least it's given with a consistent interface

### Overview

The original tiny-regex-c code has been significantly refactored, to:

* Remove all references to `NULL` character string termination, and replace them with Fortran's string intrinsics (`len`, `len_trim`, etc.)
* Remove all C escaped characters (`\n`, `\t`, etc), replace with Fortran syntax.
* Even in presence of strings, use `pure` `elemental` functions wherever possible
* It is a standalone module that has no external dependencies besides compiler modules.

### Example programs

```fortran
! Demonstrate use of regex
program test_regex
   use regex_module
   implicit none
   
   integer :: idx,ln
   character(*), parameter :: text = 'table football'
   
   
   
   idx = REGEX(string=text,pattern='foo*',length=ln);

   ! Prints "foo"
   print *, text(idx:idx+ln-1)
   
end program
```


```fortran

! Demonstrate use of object-oriented interface
program test_regex
   use regex_module
   implicit none
   
   integer :: idx,ln
   character(*), parameter :: text = 'table football'
   type(regex_op) :: re
   
   ! Parse pattern into a regex structure
   re = parse_pattern('foo*')
   
   idx = REGEX(string=text,pattern=re,length=ln);

   ! Prints "foo"
   print *, text(idx:idx+ln-1)
   
end program
   
```

### To do list

 - [ ] Add a `BACK` optional keyword to return the last instance instead of the first.
 - [ ] Option to return ALL instances as an array, instead of the first/last one only.
 - [ ] Replace fixed-size static storage with allocatable character strings (slower?)
 
### Reporting problems

Please report any problems! It is appreciated. The original C library had hacks to account for the fact that several special characters are read in with escaped sequences, which partially collides with the escaped sequence options in regex. So, expect the current API to be still a bit rough around the edges.

### License

fortran-regex is released under the MIT license. The code it's based upon is in the public domain.
