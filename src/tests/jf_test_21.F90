!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/20/2016
!
! Module for the 21st unit test.
! This one is testing the real format options.

module jf_test_21_mod

    use json_module, RK => json_RK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

    private
    public :: test_21

    character(len=*),parameter :: dir = '../files/'    !! working directory

contains

    subroutine test_21(error_cnt)

    !! Test some of the edge cases, and incorrect usages.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_file) :: jfile
    type(json_value),pointer :: ptr

    logical :: found
    real(kind=RK),dimension(:),allocatable :: array1,array2

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 21'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    array1 = [sqrt(2.0_RK),sqrt(3.0_RK),sqrt(7.0_RK),&
              sqrt(2.0e4_RK),sqrt(3.0e5_RK),sqrt(7.0e6_RK),&
              huge(1.0_RK),tiny(1.0_RK),epsilon(1.0_RK)]

    ! write array1 into file
    call json % initialize(real_format='E')
    call json % create_object(ptr,'')
    call json % add(ptr,'value',array1)
    call json % print(ptr,dir//'test21.json')
    call json % destroy(ptr)

    ! read array2 from file
    call jfile % initialize(real_format='E')
    call jfile % load(dir//'test21.json')
    call jfile % get('value',array2,found)
    call jfile % destroy()

    ! Compare
    write(*,*) ''
    if (any(array1 /= array2)) then
        error_cnt = error_cnt + 1
        write(*,*) 'Test failed'
        write(*,*) array1 == array2
    else
        write(*,*) 'Test passed'
    end if
    write(*,'(A,1X,*(E30.18E3))') 'original array :',array1
    write(*,'(A,1X,*(E30.18E3))') 'read from file :',array2
    write(*,*) ''

    end subroutine test_21

end module jf_test_21_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_21

    !! 21st unit test.

    use jf_test_21_mod, only: test_21
    implicit none
    integer :: n_errors
    call test_21(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_21
!*****************************************************************************************
#endif
