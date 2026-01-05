!  The argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_dummy_arguments.inc

logical(LK),intent(in),optional :: verbose
  !! mainly useful for debugging (default is false)
logical(LK),intent(in),optional :: compact_reals
  !! to compact the real number strings for output (default is true)
logical(LK),intent(in),optional :: print_signs
  !! always print numeric sign (default is false)
character(kind=CDK,len=*),intent(in),optional :: real_format
  !! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
integer(IK),intent(in),optional :: spaces_per_tab
  !! number of spaces per tab for indenting (default is 2)
logical(LK),intent(in),optional :: strict_type_checking
  !! if true, no integer, double, or logical type
  !! conversions are done for the `get` routines
  !! (default is false).
logical(LK),intent(in),optional :: trailing_spaces_significant
  !! for name and path comparisons, is trailing
  !! space to be considered significant.
  !! (default is false)
logical(LK),intent(in),optional :: case_sensitive_keys
  !! for name and path comparisons, are they
  !! case sensitive. (default is true)
logical(LK),intent(in),optional :: no_whitespace
  !! if true, printing the JSON structure is
  !! done without adding any non-significant
  !! spaces or linebreaks (default is false)
logical(LK),intent(in),optional :: unescape_strings
  !! If false, then the raw escaped
  !! string is returned from [[json_get_string]]
  !! and similar routines. If true [default],
  !! then the string is returned unescaped.
character(kind=CK,len=*),intent(in),optional :: comment_char
  !! If present, these characters are used
  !! to denote comments in the JSON file,
  !! which will be ignored if present.
  !! Example: `!`, `#`, or `/!#`. Setting this
  !! to a blank string disables the
  !! ignoring of comments. (Default is `/!#`).
integer(IK),intent(in),optional :: path_mode
  !! How the path strings are interpreted in the
  !! `get_by_path` routines:
  !!
  !! * 1 : Default mode (see [[json_get_by_path_default]])
  !! * 2 : as RFC 6901 "JSON Pointer" paths
  !!   (see [[json_get_by_path_rfc6901]])
  !! * 3 : JSONPath "bracket-notation"
  !!   see [[json_get_by_path_jsonpath_bracket]])
character(kind=CK,len=1),intent(in),optional :: path_separator
  !! The `path` separator to use
  !! in the "default" mode for
  !! the paths in the various
  !! `get_by_path` routines.
  !! Example: `.` [default] or `%`.
  !! Note: if `path_mode/=1`
  !! then this is ignored.
logical(LK),intent(in),optional :: compress_vectors
  !! If true, then arrays of integers,
  !! nulls, doubles, and logicals are
  !! printed all on one line.
  !! [Note: `no_whitespace` will
  !! override this option if necessary].
  !! (Default is False).
logical(LK),intent(in),optional :: allow_duplicate_keys
  !! * If True [default] then no special checks
  !!   are done to check for duplicate keys.
  !! * If False, then after parsing, if any duplicate
  !!   keys are found, an error is thrown. A call to
  !!   [[json_value_validate]] will also check for
  !!   duplicates.
logical(LK),intent(in),optional :: escape_solidus
  !! * If True then the solidus "`/`" is always escaped
  !!   "`\/`" when serializing JSON
  !! * If False [default], then it is not escaped.
  !!
  !! Note that this option does not affect parsing
  !! (both escaped and unescaped are still valid in
  !! all cases).
logical(LK),intent(in),optional :: stop_on_error
  !! If an exception is raised, then immediately quit.
  !! (Default is False).
integer(IK),intent(in),optional :: null_to_real_mode
  !! if `strict_type_checking=false`:
  !!
  !! * 1 : an exception will be raised if
  !!   try to retrieve a `null` as a real.
  !! * 2 : a `null` retrieved as a real
  !!   will return a NaN. [default]
  !! * 3 : a `null` retrieved as a real
  !!   will return 0.0.
integer(IK),intent(in),optional :: non_normal_mode
  !! How to serialize NaN, Infinity, and
  !! -Infinity real values:
  !!
  !! * 1 : as strings (e.g., "NaN",
  !!   "Infinity", "-Infinity") [default]
  !! * 2 : as JSON `null` values
logical(LK),intent(in),optional :: use_quiet_nan
  !! * If true [default], `null_to_real_mode=2`
  !!   and [[string_to_real]] will use
  !!   `ieee_quiet_nan` for NaN values.
  !! * If false,
  !!   `ieee_signaling_nan` will be used.
logical(LK),intent(in),optional :: strict_integer_type_checking
  !! * If false, when parsing JSON, if an integer numeric value
  !!   cannot be converted to an integer (`integer(IK)`),
  !!   then an attempt is then make to convert it
  !!   to a real (`real(RK)`).
  !! * If true, an exception will be raised if the integer
  !!   value cannot be read.
  !!
  !! (default is true)
logical(LK),intent(in),optional :: allow_trailing_comma
  !! Allow a single trailing comma in arrays and objects.
  !! (default is true)
integer(IK),intent(in),optional :: parser_mode
  !! Which version of the JSON parser to use:
  !!
  !! * 1 : the original recursive version [default]
  !! * 2 : a non-recursive version that uses an
  !!   explicit stack to avoid deep recursion issues.
integer(IK),intent(in),optional :: parser_initial_stack_size
  !! The initial stack size (in number of elements)
  !! to use when `parser_mode=2` (non-recursive parser).
  !! (default is 32)
integer(IK),intent(in),optional :: parser_max_stack_size
  !! The maximum stack size (in number of elements)
  !! to use when `parser_mode=2` (non-recursive parser).
  !! (default is 10000)
integer(IK),intent(in),optional :: string_to_real_mode
  !! Which implementation to use for converting
  !! strings to real values:
  !!
  !! * 1 : use the Fortran implementation [default]
  !! * 2 : use C library routines to convert
  !!   strings to real values. This may be faster
  !!   than the Fortran implementation. If the
  !!   conversion fails, it will fall back to
  !!   the Fortran version.
integer(IK),intent(in),optional :: null_to_integer_mode
  !! if `strict_type_checking=false`:
  !!
  !! * 1 : an exception will be raised if
  !!   try to retrieve a `null` as an integer. [default]
  !! * 2 : a `null` retrieved as an integer
  !!   will return a 0.
  !! * 3 : a `null` retrieved as an integer
  !!   will return the value specified in
  !!   `null_to_integer_value` (default is 0).
integer(IK),intent(in),optional :: null_to_integer_value
  !! The integer value to return when retrieving
  !! a `null` as an integer and
  !! `null_to_integer_mode=3`.
  !! (default is 0).