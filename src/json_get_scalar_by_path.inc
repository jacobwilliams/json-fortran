    type(json_value),pointer :: p

    if (present(default)) then
        value = default
    else
        value = default_if_not_specified
    end if

    if ( json%exception_thrown ) then
       call flag_not_found(found)
       return
    end if

    nullify(p)
    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call json%throw_exception('Error in '//routine//':'//&
                                  ' Unable to resolve path: '// trim(path),found)
    else
        call json%get(p,value)
    end if

    if ( json%exception_thrown ) then
        if ( present(found) .or. present(default)) then
            call flag_not_found(found)
            if (present(default)) value = default
            call json%clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if
