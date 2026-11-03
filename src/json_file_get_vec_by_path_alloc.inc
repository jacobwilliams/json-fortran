    type(json_value),pointer :: p

    if ( me%failed() ) then
        if (present(default)) then
            vec = default
            if (present(default_ilen)) then
                ilen = default_ilen
            else
                allocate(ilen(size(default)))
                ilen = len(default)
            end if
        end if
        call flag_not_found(found)
        return
    end if

    nullify(p)
    call me%get(path=path, p=p)

    if (.not. associated(p)) then
        call me%throw_exception('Error in '//routine//':'//&
                                  ' Unable to resolve path: '// trim(path),found)
    else
        call me%core%get(p,vec,ilen)
    end if

    if ( me%failed() ) then
        if ( present(found) .or. present(default)) then
            call flag_not_found(found)
            if (present(default)) then
                vec = default
                if (present(default_ilen)) then
                    ilen = default_ilen
                else
                    allocate(ilen(size(default)))
                    ilen = len(default)
                end if
            end if
            call me%clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if
