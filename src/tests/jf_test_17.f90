!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/29/2017
!
! Module for the 17th unit test.
! Test the `rename` function.

module jf_test_17_mod

    use json_module, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

contains

    subroutine test_17(error_cnt)

    !! Test the `rename` function.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,q

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 17'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%parse(p, '{"city": ["New York","Los Angeles","Chicago"], '//&
                       '"value": 1, "iflag": true, "struct":{"vec":[1,2,3]}}')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,error_unit)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Rename: "city" to "cities"'
    call json%get(p,'city',q)
    call json%rename(q,'cities')
    call json%print(p,output_unit)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(q)
    !verifty that it was renamed:
    call json%get(p,'cities',q)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') 'Success!'
    end if
    nullify(q)

    !cleanup:
    call json%destroy(p)

    end subroutine test_17

end module jf_test_17_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_17

    !! 17th unit test.

    use jf_test_17_mod, only: test_17
    implicit none
    integer :: n_errors
    call test_17(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_17
!*****************************************************************************************
