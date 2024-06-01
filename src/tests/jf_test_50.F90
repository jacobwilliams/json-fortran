!*****************************************************************************************
!>
!  Module for the 50th unit test. See Issue #546.

module jf_test_50_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_50

contains

    subroutine test_50(error_cnt)

    !! 50th unit test. see Issue #546

    integer,intent(out) :: error_cnt

    real(wp),dimension(6,6),parameter :: pcir = reshape(&
                   [0.00_wp, 0.35_wp, 0.15_wp, 0.00_wp, 0.50_wp, 0.00_wp, &  ! these are the columns
                    1.00_wp, 0.00_wp, 0.00_wp, 0.25_wp, 0.00_wp, 0.00_wp, &
                    0.00_wp, 0.00_wp, 0.00_wp, 1.00_wp, 0.00_wp, 0.00_wp, &
                    0.00_wp, 0.90_wp, 0.55_wp, 0.00_wp, 0.00_wp, 0.90_wp, &
                    0.00_wp, 0.00_wp, 0.30_wp, 0.00_wp, 0.00_wp, 0.70_wp, &
                    0.00_wp, 0.00_wp, 0.00_wp, 1.10_wp, 0.50_wp, 0.00_wp], [6,6])

    type(json_core) :: json
    type(json_value),pointer :: p

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 50'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize(compress_vectors = .true.) ! so it will print each col on one line

    call json%create_object(p,'')   !create the root
    call json_value_add_real_vec_2d(json, p, CK_'Pcir', pcir, by_col=.true.)
    call json%print(p)
    call json%destroy(p)

    error_cnt = 0

    end subroutine test_50

    subroutine json_value_add_real_vec_2d(json, p, name, val, by_col)

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer            :: p
        character(kind=CK,len=*),intent(in) :: name   !! name of the variable
        real(wp),dimension(:,:),intent(in)  :: val    !! value
        logical,intent(in) :: by_col !! if true, write by column. if false, write by row

        type(json_value),pointer :: var
        integer(IK) :: i    !! counter

        !create a variable as an array:
        call json%create_array(var,name)

        !populate as an array of arrays:
        if (by_col) then
            do i=1,size(val,2)
                call json%add(var, CK_'', val(:,i))
            end do
        else
            do i=1,size(val,1)
                call json%add(var, CK_'', val(i,:))
            end do
        end if

        !add it:
        call json%add(p, var)

    end subroutine json_value_add_real_vec_2d

end module jf_test_50_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_50

    use jf_test_50_mod , only: test_50

    implicit none
    integer :: n_errors

    call test_50(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_50
#endif
!*****************************************************************************************

