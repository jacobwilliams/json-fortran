!*****************************************************************************************
!>
!  Module for the seventh unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_7_mod

    use json_module, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_7

contains

    subroutine test_7(error_cnt)

    !! Indent test

    implicit none

    integer,intent(out) :: error_cnt

    type(json_core) :: json       !! factory for manipulating `json_value` pointers
    type(json_value),pointer :: root,a,b,c,d,e,e1,e2,escaped_string,p
    logical :: found
    character(kind=json_CK,len=1), dimension(:), allocatable :: strvec
    character(kind=json_CK,len=:), allocatable :: string

    found=.false.
    error_cnt = 0
    call json%initialize()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 7 : indent test'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

!-----------------------
! jsonlint indention is
!-----------------------
!{
!    "a": {
!        "ints": [
!            1,
!            2,
!            3
!        ],
!        "chars": [
!            "a",
!            "b",
!            "c"
!        ]
!    },
!    "b": {
!        "c": {
!            "val1": 1066
!        }
!    },
!    "d": {
!        "val2": 1815
!    },
!    "array": [
!        {
!            "int1": 1
!        },
!        {
!            "int1": 1,
!            "int2": 2
!        }
!    ]
!    "escaped string": "\\\/\b\f\n\r\t"
!}

    !create a json structure:
    call json%create_object(root,'root')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%create_object(a,'a')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(a,'ints', [1_IK,2_IK,3_IK])
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%create_object(b,'b')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(a,'chars', ['a','b','c'])
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%get_child(a,'chars',p)
    call json%get(p,strvec)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%create_object(c,'c')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(c,'val1', 1066_IK)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%create_object(d,'d')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(d,'val2', 1815_IK)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%create_array(e,'array')   !objects in an array
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%create_object(e1,'')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(e1,'int1', 1_IK)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%create_object(e2,'')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(e2,'int1', 1_IK)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(e2,'int2', 2_IK)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(e,e1)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(e,e2)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(root,a)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(root,b)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(b,c)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(root,d)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(root,e)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(root,'escaped string',&
         '\/'//&
         achar(8)//&
         achar(12)//&
         achar(10)//&
         achar(13)//&
         achar(9))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(root,'wacky string',['trim   ','  and  ',' adjust','   left'],&
         trim_str=.true.,adjustl_str=.true.)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    nullify(a)  !don't need these anymore
    nullify(b)
    nullify(c)
    nullify(d)
    nullify(e)
    nullify(e1)
    nullify(e2)
    nullify(escaped_string)

    call json%print(root)  !print to the console
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    ! look for the 'escaped string' entry
    call json%get(root,'escaped string',escaped_string,found)
    if (json%failed() .or. .not. found) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%get(escaped_string,string)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') "Fetched unescaped 'escaped string': "//string

    ! remove the escaped string entry
    if (found) call json%remove(escaped_string,destroy=.true.)
    call json%print(root,int(error_unit,IK))  !print to stderr
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%destroy(root)  !cleanup
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_7

end module jf_test_7_mod
!*****************************************************************************************


#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_7

    !! Seventh unit test.

    use jf_test_7_mod , only: test_7
    implicit none
    integer :: n_errors
    call test_7(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_7
!*****************************************************************************************
#endif
