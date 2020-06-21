    type(json_value),pointer :: p

    if ( json%exception_thrown ) then
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
    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call json%throw_exception('Error in '//routine//':'//&
                                  ' Unable to resolve path: '// trim(path),found)
    else
        call json%get(p,vec,ilen)
    end if

    if ( json%exception_thrown ) then
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
            call json%clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if
