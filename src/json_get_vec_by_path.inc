    type(json_value),pointer :: p

    if ( json%exception_thrown ) then
        if (present(default)) vec = default
        call flag_not_found(found)
        return
    end if

    nullify(p)
    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call json%throw_exception('Error in '//routine//':'//&
                                  ' Unable to resolve path: '// trim(path),found)
    else
        call json%get(p,vec)
    end if

    if ( json%exception_thrown ) then
        if ( present(found) .or. present(default)) then
            call flag_not_found(found)
            if (present(default)) vec = default
            call json%clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if
