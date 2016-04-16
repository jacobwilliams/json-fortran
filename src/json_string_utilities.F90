!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  JSON-Fortran support module for string manipulation.

    module json_string_utilities

    use json_kinds

    implicit none

    private

    character(kind=CK,len=*),parameter,public :: star = '*' !! for invalid numbers and
                                                            !! list-directed real output

    !******************************************************
    !>
    !  Convert a 'DEFAULT' kind character input to
    !  'ISO_10646' kind and return it
    interface to_unicode
        module procedure to_uni, to_uni_vec
    end interface
    !******************************************************

# ifdef USE_UCS4
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
    ! Provide a string comparison operator that works
    ! with mixed kinds
    interface operator(==)
       module procedure ucs4_comp_default, default_comp_ucs4
    end interface
    public :: operator(==)
    !******************************************************
# endif

    public :: integer_to_string
    public :: real_to_string
    public :: valid_json_hex
    public :: to_unicode

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
    character(kind=CK,len=*),intent(out) :: str     !! ival converted to a string.

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
    character(kind=CK,len=*),intent(out) :: str          !! rval converted to a string.

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

    character(kind=CK,len=len(str)) :: significand, expnt
    character(kind=CK,len=2) :: separator
    integer(IK) :: exp_start,decimal_pos,sig_trim,exp_trim,i

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

    integer(IK) :: n,i

    !an array of the valid hex characters:
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
!  CK//CDK operator.

    function ucs4_join_default(ucs4_str,def_str) result(res)

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
!  CDK//CK operator.

    function default_join_ucs4(def_str,ucs4_str) result(res)

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
!  CK==CDK operator.

    function ucs4_comp_default(ucs4_str,def_str) result(res)

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
!  CDK==CK operator.

    function default_comp_ucs4(def_str,ucs4_str) result(res)

    implicit none

    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK, len=*), intent(in) :: ucs4_str
    logical(LK) :: res

    res = (to_unicode(def_str) == ucs4_str)

    end function default_comp_ucs4
!*****************************************************************************************

    end module json_string_utilities
!*****************************************************************************************
