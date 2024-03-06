!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Other parameters used by JSON-Fortran.
!  This is a low-level module not meant to be used by a JSON-Fortran user.
!
!### License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    module json_parameters

    use json_kinds

    implicit none

    public

    character(kind=CDK,len=*),parameter :: json_ext = '.json'   !! JSON file extension

    ! The types of JSON data.
    integer(IK),parameter :: json_unknown   = 0  !! Unknown JSON data type
                                                 !! (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter :: json_null      = 1  !! Null JSON data type
                                                 !! (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter :: json_object    = 2  !! Object JSON data type
                                                 !! (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter :: json_array     = 3  !! Array JSON data type
                                                 !! (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter :: json_logical   = 4  !! Logical JSON data type (`logical(LK)`)
                                                 !! (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter :: json_integer   = 5  !! Integer JSON data type (`integer(IK)`)
                                                 !! (see [[json_file_variable_info]] and [[json_info]]).
    integer(IK),parameter :: json_real      = 6  !! Real number JSON data type (`real(RK)`)
                                                 !! (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter :: json_string    = 7  !! String JSON data type (`character(kind=CK)`)
                                                 !! (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter :: json_double    = json_real  !! Equivalent to `json_real` for
                                                         !! backward compatibility.

    !special JSON characters
    character(kind=CK,len=*),parameter :: space           = CK_' '  !! space character
    character(kind=CK,len=*),parameter :: start_object    = CK_'{'  !! start of a JSON object
    character(kind=CK,len=*),parameter :: end_object      = CK_'}'  !! end of a JSON object
    character(kind=CK,len=*),parameter :: start_array     = CK_'['  !! start of a JSON array
    character(kind=CK,len=*),parameter :: end_array       = CK_']'  !! end of a JSON array
    character(kind=CK,len=*),parameter :: delimiter       = CK_','  !! delimiter for JSON
    character(kind=CK,len=*),parameter :: colon_char      = CK_':'  !! colon character for JSON
    character(kind=CK,len=*),parameter :: start_array_alt = CK_'('  !! alternate start of JSON array for
                                                                    !! [[json_get_by_path_default]]
    character(kind=CK,len=*),parameter :: end_array_alt   = CK_')'  !! alternate end of JSON array for
                                                                    !! [[json_get_by_path_default]]
    character(kind=CK,len=*),parameter :: root            = achar(36, kind=CK)  !! (`$`) root for [[json_get_by_path_default]]
    character(kind=CK,len=*),parameter :: this            = CK_'@'  !! 'this' for [[json_get_by_path_default]]
    character(kind=CK,len=*),parameter :: dot             = CK_'.'  !! path separator for [[json_get_by_path_default]]
    character(kind=CK,len=*),parameter :: tilde           = CK_'~'  !! RFC 6901 escape character
    character(kind=CK,len=*),parameter :: single_quote    = CK_"'"  !! for JSONPath bracket-notation
    character(kind=CK,len=*),parameter :: quotation_mark  = CK_'"'  !! JSON special character
    character(kind=CK,len=*),parameter :: bspace          = achar(8,  kind=CK) !! JSON special character
    character(kind=CK,len=*),parameter :: horizontal_tab  = achar(9,  kind=CK) !! JSON special character
    character(kind=CK,len=*),parameter :: newline         = achar(10, kind=CK) !! JSON special character
    character(kind=CK,len=*),parameter :: formfeed        = achar(12, kind=CK) !! JSON special character
    character(kind=CK,len=*),parameter :: carriage_return = achar(13, kind=CK) !! JSON special character
    character(kind=CK,len=*),parameter :: slash           = achar(47, kind=CK) !! JSON special character
    character(kind=CK,len=*),parameter :: backslash       = achar(92, kind=CK) !! JSON special character

    !> default real number format statement (for writing real values to strings and files).
    !  Note that this can be overridden by calling [[json_initialize]].
#ifdef REAL32
    character(kind=CDK,len=*),parameter :: default_real_fmt = '(ss,E17.8E3)'
#elif REAL128
    character(kind=CDK,len=*),parameter :: default_real_fmt = '(ss,E46.35E5)'
#else
    character(kind=CDK,len=*),parameter :: default_real_fmt = '(ss,E27.17E4)'
#endif

    character(kind=CK,len=*),parameter :: star = CK_'*' !! for invalid numbers and
                                                        !! list-directed real output

#if defined __GFORTRAN__
    !not parameters due to gfortran bug (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=65141)
    character(kind=CK,len=26),protected :: upper = CK_'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !! uppercase characters
    character(kind=CK,len=26),protected :: lower = CK_'abcdefghijklmnopqrstuvwxyz' !! lowercase characters
#else
    character(kind=CK,len=*),parameter :: upper = CK_'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !! uppercase characters
    character(kind=CK,len=*),parameter :: lower = CK_'abcdefghijklmnopqrstuvwxyz' !! lowercase characters
#endif

#if defined __GFORTRAN__
    !not parameters due to gfortran bug (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=65141)
    character(kind=CK,len=4),protected :: null_str  = CK_'null'  !! JSON Null variable string
    character(kind=CK,len=4),protected :: true_str  = CK_'true'  !! JSON logical True string
    character(kind=CK,len=5),protected :: false_str = CK_'false' !! JSON logical False string
#else
    character(kind=CK,len=*),parameter :: null_str  = CK_'null'  !! JSON Null variable string
    character(kind=CK,len=*),parameter :: true_str  = CK_'true'  !! JSON logical True string
    character(kind=CK,len=*),parameter :: false_str = CK_'false' !! JSON logical False string
#endif

    integer, private :: i_      !! just a counter for `control_chars` array
    character(kind=CK,len=*),dimension(32),parameter :: control_chars = &
        [(achar(i_,kind=CK),i_=1,31), achar(127,kind=CK)] !! Control characters, possibly in unicode

    !find out the precision of the floating point number system
    !and set safety factors
    integer(IK),parameter :: rp_safety_factor = 1_IK
    integer(IK),parameter :: rp_addl_safety = 2_IK
    integer(IK),parameter :: real_precision = rp_safety_factor*precision(1.0_RK) + &
                                              rp_addl_safety

    !Get the number of possible digits in the exponent when using decimal number system
    integer(IK),parameter :: maxexp = maxexponent(1.0_RK)
    integer(IK),parameter :: minexp = minexponent(1.0_RK)
    integer(IK),parameter :: real_exponent_digits = floor( 1_IK + log10( &
                                  real(max(maxexp,abs(maxexp)),&
                                  kind=RK) ) )

    integer(IK),parameter :: max_numeric_str_len = real_precision + real_exponent_digits + 6_IK
        !! 6 = sign + leading 0 + decimal + 'E' + exponent sign + 1 extra
    character(kind=CDK,len=*),parameter :: int_fmt  = '(ss,I0)' !! minimum width format for integers

    integer(IK),parameter :: max_integer_str_len = 256_IK !! maximum string length of an integer.
                                                          !! This is totally arbitrary (any way
                                                          !! to get the compiler to tell us this?)

    integer(IK),parameter :: chunk_size = 256_IK  !! for allocatable strings: allocate chunks of this size
    integer(IK),parameter :: unit2str = -1_IK  !! unit number to cause stuff to be
                                               !! output to strings rather than files.
                                               !! See 9.5.6.12 in the F2003/08 standard
    character(kind=CK,len=*),parameter :: blank_chunk = repeat(space, chunk_size) !! a blank string

    integer(IK),parameter :: seq_chunk_size = 256_IK !! chunk size for reading sequential files

    integer(IK),parameter :: stream_chunk_size = 256_IK !! chunk size for reading stream files

    integer(IK),parameter :: print_str_chunk_size = 1000_IK !! chunk size for writing JSON to a string

    integer(IK),parameter :: pushed_char_size = 10_IK !! size for `pushed_char`
                                                      !! array in [[json_core(type)]]

    end module json_parameters
!*****************************************************************************************
