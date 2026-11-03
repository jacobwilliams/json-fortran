    type(json_value),pointer :: p

    if (present(default)) then
        val = default
    else
        val = default_if_not_specified
    end if

    if ( me%failed() ) then
       call flag_not_found(found)
       return
    end if

    nullify(p)
    call me%get(path=path, p=p)

    if (.not. associated(p)) then
        call me%throw_exception('Error in '//routine//':'//&
                                ' Unable to resolve path: '// trim(path),found)
    else
        call me%core%get(p,val)
    end if

    if ( me%failed() ) then
        if ( present(found) .or. present(default)) then
            call flag_not_found(found)
            if (present(default)) val = default
            call me%clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if
