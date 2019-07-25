!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
! Module for the 16th unit test.
! Test the `swap` function.

module jf_test_16_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

    private
    public :: test_16

contains

    subroutine test_16(error_cnt)

    !! Test the `swap` function.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,p1,p2

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 16'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%deserialize(p, '{"cities": ["New York","Los Angeles","Chicago"], '//&
                       '"value": 1, "iflag": true, "struct":{"vec":[1,2,3]}}')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(error_unit,IK))

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: cities <-> iflag'
    call json%get(p,'cities',p1)
    call json%get(p,'iflag',p2)
    call json%swap(p1,p2)
    call json%print(p,int(output_unit,IK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(p1)
    nullify(p2)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: iflag <-> value'
    call json%get(p,'iflag',p1)
    call json%get(p,'value',p2)
    call json%swap(p1,p2)
    call json%print(p,int(output_unit,IK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(p1)
    nullify(p2)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: iflag <-> struct.vec'
    call json%get(p,'iflag',p1)
    call json%get(p,'struct.vec',p2)
    call json%swap(p1,p2)
    call json%print(p,int(output_unit,IK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(p1)
    nullify(p2)

    call json%destroy(p)

    !...........................................................................
    ! another case

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '.....................................'
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%deserialize(p, '{ "stats": { "iflag": 0, "str": "ok" },'//&
                             '"vars": [{ "label": "r", "value": 0.0 }, '//&
                                      '{ "label": "v", "value": 0.0 }],'//&
                             '"empty": { } }')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(error_unit,IK))

    !this one is not allowed, and should fail:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: vars(1).label <-> vars'
    call json%get(p,'vars(1).label',p1)
    call json%get(p,'vars',p2)
    call json%swap(p1,p2)
    call json%print(p,int(output_unit,IK))
    if (.not. json%failed()) then
        write(error_unit,'(A)') 'Error: this should have failed.'
        error_cnt = error_cnt + 1
    else
        call json%clear_exceptions()
    end if
    nullify(p1)
    nullify(p2)

    !this one should work:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: empty <-> stats.str'
    call json%get(p,'empty',p1)
    call json%get(p,'stats.str',p2)
    call json%swap(p1,p2)
    call json%print(p,int(output_unit,IK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(p1)
    nullify(p2)

    call json%destroy(p)

    !...........................................................................
    ! other special cases:

    !swap first and last items in a list
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '.....................................'
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%deserialize(p, '{ "color": "red", "width": 10, "height": 2 }')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(error_unit,IK))
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: color <-> height'
    call json%get(p,'color',p1)
    call json%get(p,'height',p2)
    call json%swap(p1,p2)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(output_unit,IK))
    nullify(p1)
    nullify(p2)
    call json%destroy(p)

    !p2 is first child:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '.....................................'
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%deserialize(p, '{ "color": "red", "width": 10, "height": 2 }')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(error_unit,IK))
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: width <-> color'
    call json%get(p,'width',p1)
    call json%get(p,'color',p2)
    call json%swap(p1,p2)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(output_unit,IK))
    nullify(p1)
    nullify(p2)
    call json%destroy(p)

    !p2 is last child:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '.....................................'
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%deserialize(p, '{ "color": "red", "width": 10, "height": 2 }')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(error_unit,IK))
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: width <-> height'
    call json%get(p,'width',p1)
    call json%get(p,'height',p2)
    call json%swap(p1,p2)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(output_unit,IK))
    nullify(p1)
    nullify(p2)
    call json%destroy(p)

    end subroutine test_16

end module jf_test_16_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_16

    !! 16th unit test.

    use jf_test_16_mod, only: test_16
    implicit none
    integer :: n_errors
    call test_16(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_16
!*****************************************************************************************
#endif
