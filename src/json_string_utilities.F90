!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  JSON-Fortran support module for string manipulation.
!
!## License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    module json_string_utilities

    use json_kinds
    use json_parameters

    implicit none

    private

    !******************************************************
    !>
    !  Convert a 'DEFAULT' kind character input to
    !  'ISO_10646' kind and return it
    interface to_unicode
        module procedure to_uni, to_uni_vec
    end interface
    !******************************************************

#ifdef USE_UCS4
    !******************************************************
    !>
    ! Provide a means to convert to UCS4 while
    ! concatenating UCS4 and default strings
    interface operator(//)
       module procedure ucs4_join_default, default_join_ucs4
    end interface
    public :: operator(//)
    !******************************************************

    !******************************************************
    !>
    ! Provide a string `==` operator that works
    ! with mixed kinds
    interface operator(==)
       module procedure ucs4_comp_default, default_comp_ucs4
    end interface
    public :: operator(==)
    !******************************************************

    !******************************************************
    !>
    ! Provide a string `/=` operator that works
    ! with mixed kinds
    interface operator(/=)
       module procedure ucs4_neq_default, default_neq_ucs4
    end interface
    public :: operator(/=)
    !******************************************************
#endif

    public :: integer_to_string
    public :: real_to_string
    public :: string_to_integer
    public :: valid_json_hex
    public :: to_unicode
    public :: escape_string
    public :: unescape_string
    public :: lowercase_string
    public :: replace_string
    public :: decode_rfc6901
    public :: encode_rfc6901

    contains
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Convert an integer to a string.

    pure subroutine integer_to_string(ival,int_fmt,str)

    implicit none

    integer(IK),intent(in)               :: ival    !! integer value.
    character(kind=CDK,len=*),intent(in) :: int_fmt !! format for integers
    character(kind=CK,len=*),intent(out) :: str     !! `ival` converted to a string.

    integer(IK) :: istat

    write(str,fmt=int_fmt,iostat=istat) ival

    if (istat==0) then
        str = adjustl(str)
    else
        str = repeat(star,len(str))
    end if

    end subroutine integer_to_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string into an integer.
!
!# History
!  * Jacob Williams : 12/10/2013 : Rewrote original `parse_integer` routine.
!    Added error checking.
!  * Modified by Izaak Beekman
!  * Jacob Williams : 2/4/2017 : moved core logic to this routine.

    subroutine string_to_integer(str,ival,status_ok)

    implicit none

    character(kind=CK,len=*),intent(in) :: str        !! the string to conver to an integer
    integer(IK),intent(out)             :: ival       !! the integer value
    logical(LK),intent(out)             :: status_ok  !! true if there were no errors

    character(kind=CDK,len=:),allocatable :: digits
    integer(IK) :: ndigits_digits,ndigits,ierr

    ! Compute how many digits we need to read
    ndigits = 2*len_trim(str)
    ndigits_digits = floor(log10(real(ndigits)))+1
    allocate(character(kind=CDK,len=ndigits_digits) :: digits)
    write(digits,'(I0)') ndigits !gfortran will have a runtime error with * edit descriptor here
    ! gfortran bug: '*' edit descriptor for ISO_10646 strings does bad stuff.
    read(str,'(I'//trim(digits)//')',iostat=ierr) ival   !string to integer

    ! error check:
    status_ok = (ierr==0)
    if (.not. status_ok) ival = 0_IK

    end subroutine string_to_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Convert a real value to a string.
!
!# Modified
!  * Izaak Beekman : 02/24/2015 : added the compact option.
!  * Jacob Williams : 10/27/2015 : added the star option.

    subroutine real_to_string(rval,real_fmt,compact_real,str)

    implicit none

    real(RK),intent(in)                  :: rval         !! real value.
    character(kind=CDK,len=*),intent(in) :: real_fmt     !! format for real numbers
    logical(LK),intent(in)               :: compact_real !! compact the string so that it is
                                                         !! displayed with fewer characters
    character(kind=CK,len=*),intent(out) :: str          !! `rval` converted to a string.

    integer(IK) :: istat

    if (real_fmt==star) then
        write(str,fmt=*,iostat=istat) rval
    else
        write(str,fmt=real_fmt,iostat=istat) rval
    end if

    if (istat==0) then
        !in this case, the default string will be compacted,
        ! so that the same value is displayed with fewer characters.
        if (compact_real) call compact_real_string(str)
    else
        str = repeat(star,len(str))
    end if

    end subroutine real_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 02/24/2015
!
!  Compact a string representing a real number, so that
!  the same value is displayed with fewer characters.
!
!# See also
!  * [[real_to_string]]

    subroutine compact_real_string(str)

    implicit none

    character(kind=CK,len=*),intent(inout) :: str  !! string representation of a real number.

    character(kind=CK,len=len(str)) :: significand
    character(kind=CK,len=len(str)) :: expnt
    character(kind=CK,len=2) :: separator
    integer(IK) :: exp_start
    integer(IK) :: decimal_pos
    integer(IK) :: sig_trim
    integer(IK) :: exp_trim
    integer(IK) :: i  !! counter

    str = adjustl(str)
    exp_start = scan(str,CK_'eEdD')
    if (exp_start == 0) exp_start = scan(str,CK_'-+',back=.true.)
    decimal_pos = scan(str,CK_'.')
    if (exp_start /= 0) separator = str(exp_start:exp_start)

    if ( exp_start < decimal_pos ) then !possibly signed, exponent-less float

        significand = str
        sig_trim = len(trim(significand))
        do i = len(trim(significand)),decimal_pos+2,-1 !look from right to left at 0s
                                                       !but save one after the decimal place
            if (significand(i:i) == '0') then
                sig_trim = i-1
            else
                exit
            end if
        end do
        str = trim(significand(1:sig_trim))

    else if (exp_start > decimal_pos) then !float has exponent

        significand = str(1:exp_start-1)
        sig_trim = len(trim(significand))
        do i = len(trim(significand)),decimal_pos+2,-1 !look from right to left at 0s
            if (significand(i:i) == '0') then
                sig_trim = i-1
            else
                exit
            end if
        end do
        expnt = adjustl(str(exp_start+1:))
        if (expnt(1:1) == '+' .or. expnt(1:1) == '-') then
            separator = trim(adjustl(separator))//expnt(1:1)
            exp_start = exp_start + 1
            expnt     = adjustl(str(exp_start+1:))
        end if
        exp_trim = 1
        do i = 1,(len(trim(expnt))-1) !look at exponent leading zeros saving last
            if (expnt(i:i) == '0') then
                exp_trim = i+1
            else
                exit
            end if
        end do
        str = trim(adjustl(significand(1:sig_trim)))// &
              trim(adjustl(separator))// &
              trim(adjustl(expnt(exp_trim:)))

    !else ! mal-formed real, BUT this code should be unreachable

    end if

    end subroutine compact_real_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2014
!
!  Add the escape characters to a string for adding to JSON.

    subroutine escape_string(str_in, str_out)

    implicit none

    character(kind=CK,len=*),intent(in)              :: str_in
    character(kind=CK,len=:),allocatable,intent(out) :: str_out

    integer(IK) :: i    !! counter
    integer(IK) :: ipos !! accumulated string size
                        !! (so we can allocate it in chunks for
                        !! greater runtime efficiency)
    character(kind=CK,len=1) :: c  !! for reading `str_in` one character at a time.
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: tmp !! workaround for bug in gfortran 6.1
#endif

    character(kind=CK,len=*),parameter :: specials = quotation_mark//&
                                                     backslash//&
                                                     slash//&
                                                     bspace//&
                                                     formfeed//&
                                                     newline//&
                                                     carriage_return//&
                                                     horizontal_tab

    !Do a quick scan for the special characters,
    ! if any are present, then process the string,
    ! otherwise, return the string as is.
    if (scan(str_in,specials)>0) then

        str_out = repeat(space,chunk_size)
        ipos = 1

        !go through the string and look for special characters:
        do i=1,len(str_in)

            c = str_in(i:i)    !get next character in the input string

            !if the string is not big enough, then add another chunk:
            if (ipos+3>len(str_out)) str_out = str_out // repeat(space, chunk_size)

            select case(c)
            case(quotation_mark,backslash,slash)
                str_out(ipos:ipos+1) = backslash//c
                ipos = ipos + 2
            case(bspace)
                str_out(ipos:ipos+1) = '\b'
                ipos = ipos + 2
            case(formfeed)
                str_out(ipos:ipos+1) = '\f'
                ipos = ipos + 2
            case(newline)
                str_out(ipos:ipos+1) = '\n'
                ipos = ipos + 2
            case(carriage_return)
                str_out(ipos:ipos+1) = '\r'
                ipos = ipos + 2
            case(horizontal_tab)
                str_out(ipos:ipos+1) = '\t'
                ipos = ipos + 2
            case default
                str_out(ipos:ipos) = c
                ipos = ipos + 1
            end select

        end do

        !trim the string if necessary:
        if (ipos<len(str_out)+1) then
            if (ipos==1) then
                str_out = ''
            else
#if defined __GFORTRAN__
                tmp = str_out(1:ipos-1)      !workaround for bug in gfortran 6.1
                str_out = tmp
#else
                str_out = str_out(1:ipos-1)  !original
#endif
            end if
        end if

    else

        str_out = str_in

    end if

    end subroutine escape_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Remove the escape characters from a JSON string and return it.
!
!  The escaped characters are denoted by the '\' character:
!````
!    '\"'        quotation mark
!    '\\'        reverse solidus
!    '\/'        solidus
!    '\b'        backspace
!    '\f'        formfeed
!    '\n'        newline (LF)
!    '\r'        carriage return (CR)
!    '\t'        horizontal tab
!    '\uXXXX'    4 hexadecimal digits
!````

    subroutine unescape_string(str_in, str_out, error_message)

    implicit none

    character(kind=CK,len=*),intent(in)              :: str_in  !! string as stored in a [[json_value]]
    character(kind=CK,len=:),allocatable,intent(out) :: str_out !! decoded string
    character(kind=CK,len=:),allocatable,intent(out) :: error_message !! will be allocated if there was an error

    integer :: i   !! counter
    integer :: n   !! length of str_in
    integer :: m   !! length of str_out
    character(kind=CK,len=1) :: c  !! for scanning each character in string

#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: tmp  !! for GFortran bug workaround
#endif

    if (scan(str_in,backslash)>0) then

        !there is at least one escape character, so process this string:

        n = len(str_in)
        str_out = repeat(space,n) !size the output string (will be trimmed later)
        m = 0  !counter in str_out
        i = 0  !counter in str_in

        do

            i = i + 1
            if (i>n) exit ! finished
            c = str_in(i:i) ! get next character in the string

            if (c == backslash) then

                if (i<n) then

                    i = i + 1
                    c = str_in(i:i) !character after the escape

                    if (any(c == [quotation_mark,backslash,slash, &
                         to_unicode(['b','f','n','r','t'])])) then

                        select case(c)
                        case (quotation_mark,backslash,slash)
                            !use d as is
                        case (CK_'b')
                             c = bspace
                        case (CK_'f')
                             c = formfeed
                        case (CK_'n')
                             c = newline
                        case (CK_'r')
                             c = carriage_return
                        case (CK_'t')
                             c = horizontal_tab
                        end select

                        m = m + 1
                        str_out(m:m) = c

                    else if (c == 'u') then !expecting 4 hexadecimal digits after
                                            !the escape character    [\uXXXX]

                        !for now, we are just returning them as is
                        ![not checking to see if it is a valid hex value]
                        !
                        ! Example:
                        !   123456
                        !   \uXXXX

                        if (i+4<=n) then
                            m = m + 1
                            str_out(m:m+5) = str_in(i-1:i+4)
                            i = i + 4
                            m = m + 5
                        else
                            error_message = 'Error in unescape_string:'//&
                                                 ' Invalid hexadecimal sequence'//&
                                                 ' in string: '//str_in(i-1:)
                            if (allocated(str_out)) deallocate(str_out)
                            return
                        end if

                    else
                        !unknown escape character
                        error_message = 'Error in unescape_string:'//&
                                             ' unknown escape sequence in string "'//&
                                             trim(str_in)//'" ['//backslash//c//']'
                        if (allocated(str_out)) deallocate(str_out)
                        return
                    end if

                else
                    !an escape character is the last character in
                    ! the string [this may not be valid syntax,
                    ! but just keep it]
                    m = m + 1
                    str_out(m:m) = c
                end if

            else
                m = m + 1
                str_out(m:m) = c
            end if

        end do

        !trim trailing space:
#if defined __GFORTRAN__
        ! workaround for Gfortran 6.1.0 bug
        tmp = str_out(1:m)
        str_out = tmp
#else
        str_out = str_out(1:m)
#endif

    else
        !there are no escape characters, so return as is:
        str_out = str_in
    end if

    end subroutine unescape_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date:6/14/2014
!
!  Returns true if the string is a valid 4-digit hex string.
!
!# Examples
!```fortran
!    valid_json_hex('0000')  !returns true
!    valid_json_hex('ABC4')  !returns true
!    valid_json_hex('AB')    !returns false (< 4 characters)
!    valid_json_hex('WXYZ')  !returns false (invalid characters)
!```

    pure function valid_json_hex(str) result(valid)

    implicit none

    logical(LK)                         :: valid  !! is str a value 4-digit hex string
    character(kind=CK,len=*),intent(in) :: str    !! the string to check.

    integer(IK) :: n  !! length of `str`
    integer(IK) :: i  !! counter

    !> an array of the valid hex characters
    character(kind=CK,len=1),dimension(22),parameter :: valid_chars = &
        [ (achar(i),i=48,57), & ! decimal digits
          (achar(i),i=65,70), & ! capital A-F
          (achar(i),i=97,102) ] ! lowercase a-f

    !initialize
    valid = .false.

    !check all the characters in the string:
    n = len(str)
    if (n==4) then
        do i=1,n
            if (.not. any(str(i:i)==valid_chars)) return
        end do
        valid = .true.    !all are in the set, so it is OK
    end if

    end function valid_json_hex
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Convert string to unicode (CDK to CK).

    pure function to_uni(str)

    implicit none

    character(kind=CDK,len=*), intent(in) :: str
    character(kind=CK,len=len(str))       :: to_uni

    to_uni = str

    end function to_uni
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Convert array of strings to unicode (CDK to CK).
!
!@note JW: may be able to remove this by making [[to_uni]] PURE ELEMENTAL ?

    pure function to_uni_vec(str)

    implicit none

    character(kind=CDK,len=*), dimension(:), intent(in)   :: str
    character(kind=CK,len=len(str)), dimension(size(str)) :: to_uni_vec

    to_uni_vec = str

    end function to_uni_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CK`//`CDK` operator.

    pure function ucs4_join_default(ucs4_str,def_str) result(res)

    implicit none

    character(kind=CK, len=*), intent(in) :: ucs4_str
    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK,len=(len(ucs4_str)+len(def_str))) :: res

    res = ucs4_str//to_unicode(def_str)

    end function ucs4_join_default
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CDK`//`CK` operator.

    pure function default_join_ucs4(def_str,ucs4_str) result(res)

    implicit none

    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK, len=*), intent(in) :: ucs4_str
    character(kind=CK,len=(len(def_str)+len(ucs4_str))) :: res

    res = to_unicode(def_str)//ucs4_str

    end function default_join_ucs4
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CK`==`CDK` operator.

    pure elemental function ucs4_comp_default(ucs4_str,def_str) result(res)

    implicit none

    character(kind=CK, len=*), intent(in) :: ucs4_str
    character(kind=CDK,len=*), intent(in) :: def_str
    logical(LK) :: res

    res = ( ucs4_str == to_unicode(def_str) )

    end function ucs4_comp_default
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CDK`==`CK` operator.

    pure elemental function default_comp_ucs4(def_str,ucs4_str) result(res)

    implicit none

    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK, len=*), intent(in) :: ucs4_str
    logical(LK) :: res

    res = (to_unicode(def_str) == ucs4_str)

    end function default_comp_ucs4
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  `CK`/=`CDK` operator.

    pure elemental function ucs4_neq_default(ucs4_str,def_str) result(res)

    implicit none

    character(kind=CK, len=*), intent(in) :: ucs4_str
    character(kind=CDK,len=*), intent(in) :: def_str
    logical(LK) :: res

    res = ( ucs4_str /= to_unicode(def_str) )

    end function ucs4_neq_default
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  `CDK`/=`CK` operator.

    pure elemental function default_neq_ucs4(def_str,ucs4_str) result(res)

    implicit none

    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK, len=*), intent(in) :: ucs4_str
    logical(LK) :: res

    res = (to_unicode(def_str) /= ucs4_str)

    end function default_neq_ucs4
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Return the lowercase version of the `CK` character.

    pure elemental function lowercase_character(c) result(c_lower)

    implicit none

    character(kind=CK,len=1),intent(in) :: c
    character(kind=CK,len=1)            :: c_lower

    integer :: i  !! index in uppercase array

    i = index(upper,c)
    c_lower = merge(lower(i:i),c,i>0)

    end function lowercase_character
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Returns lowercase version of the `CK` string.

    pure elemental function lowercase_string(str) result(s_lower)

    implicit none

    character(kind=CK,len=*),intent(in) :: str      !! input string
    character(kind=CK,len=(len(str)))   :: s_lower  !! lowercase version of the string

    integer :: i  !! counter
    integer :: n  !! length of input string

    s_lower = ''
    n = len_trim(str)

    if (n>0) then
        do concurrent (i=1:n)
            s_lower(i:i) = lowercase_character(str(i:i))
        end do
    end if

    end function lowercase_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Replace all occurances of `s1` in `str` with `s2`.
!
!  A case-sensitive match is used.
!
!@note `str` must be allocated.

    pure subroutine replace_string(str,s1,s2)

    implicit none

    character(kind=CK,len=:),allocatable,intent(inout) :: str
    character(kind=CK,len=*),intent(in) :: s1
    character(kind=CK,len=*),intent(in) :: s2

    character(kind=CK,len=:),allocatable :: tmp  !! temporary string for accumulating result
    integer(IK) :: i      !! counter
    integer(IK) :: n      !! for accumulating the string
    integer(IK) :: ilen   !! length of `str` string
    integer(IK) :: ilen1  !! length of `s1` string

    if (len(str)>0) then

        tmp = ''  ! initialize
        ilen1 = len(s1)

        !     .
        ! '123ab789'

        do
            ilen = len(str)
            i = index(str,s1)
            if (i>0) then
                if (i>1) tmp = tmp//str(1:i-1)
                tmp = tmp//s2 ! replace s1 with s2 in new string
                n = i+ilen1+1 ! start of remainder of str to keep
                if (n<ilen) then
                    str = str(n:ilen)
                else
                    ! done
                    exit
                end if
            else
                ! done: get remainder of string
                tmp = tmp//str
                exit
            end if
        end do

        str = tmp

    end if

    end subroutine replace_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Decode a string from the "JSON Pointer" RFC 6901 format.
!
!  It replaces `~1` with `/` and `~0` with `~`.

    pure function decode_rfc6901(str) result(str_out)

    implicit none

    character(kind=CK,len=*),intent(in) :: str
    character(kind=CK,len=:),allocatable :: str_out

    str_out = str

    call replace_string(str_out,tilde//CK_'1',slash)
    call replace_string(str_out,tilde//CK_'0',tilde)

    end function decode_rfc6901
!*****************************************************************************************

!*****************************************************************************************
!>
!  Encode a string into the "JSON Pointer" RFC 6901 format.
!
!  It replaces `~` with `~0` and `/` with `~1`.

    pure function encode_rfc6901(str) result(str_out)

    implicit none

    character(kind=CK,len=*),intent(in) :: str
    character(kind=CK,len=:),allocatable :: str_out

    str_out = str

    call replace_string(str_out,tilde,tilde//CK_'0')
    call replace_string(str_out,slash,tilde//CK_'1')

    end function encode_rfc6901
!*****************************************************************************************

    end module json_string_utilities
!*****************************************************************************************
