    logical(LK),intent(in),optional :: verbose       !! mainly useful for debugging (default is false)
    logical(LK),intent(in),optional :: compact_reals !! to compact the real number strings for output (default is true)
    logical(LK),intent(in),optional :: print_signs   !! always print numeric sign (default is false)
    character(kind=CDK,len=*),intent(in),optional :: real_format !! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
    integer(IK),intent(in),optional :: spaces_per_tab !! number of spaces per tab for indenting (default is 2)
    logical(LK),intent(in),optional :: strict_type_checking !! if true, no integer, double, or logical type
                                                            !! conversions are done for the `get` routines
                                                            !! (default is false)
    logical(LK),intent(in),optional :: trailing_spaces_significant  !! for name and path comparisons, is trailing
                                                                    !! space to be considered significant.
                                                                    !! (default is false)
    logical(LK),intent(in),optional :: case_sensitive_keys  !! for name and path comparisons, are they
                                                            !! case sensitive. (default is true)
    logical(LK),intent(in),optional :: no_whitespace  !! if true, printing the JSON structure is
                                                      !! done without adding any non-significant
                                                      !! spaces or linebreaks (default is false)
    logical(LK),intent(in),optional :: unescape_strings !! If false, then the raw escaped
                                                        !! string is returned from [[json_get_string]]
                                                        !! and similar routines. If true [default],
                                                        !! then the string is returned unescaped.
    character(kind=CK,len=1),intent(in),optional :: comment_char  !! If present, this character is used
                                                                  !! to denote comments in the JSON file,
                                                                  !! which will be ignored if present.
                                                                  !! Example: `!` or `#`.
    logical(LK),intent(in),optional :: use_rfc6901_paths  !! If true, then path values in the
                                                          !! `get_by_path` routines are interpreted
                                                          !! as RFC 6901 "JSON Pointer" paths.
                                                          !! By default, this is False.
