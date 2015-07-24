module jf_test_12_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/outputs/' !! Path to write JSON file to
    character(len=*),parameter :: file = 'array.json'       !! Filename to write
    real(wp), parameter        :: TOL = 100*epsilon(1.0_wp) !! Tolerance for real comparisons

contains

    subroutine test_12(error_cnt)

    integer,intent(out) :: error_cnt !! report number of errors to caller

    integer,parameter :: imx = 5, jmx = 3, kmx = 4 !! dimensions for raw work array of primitive type
    integer :: shape(3)                            !! shape of work array
    integer, allocatable :: fetched_shape(:)       !! retrieved shape
    type(json_value), pointer :: root, meta_array  !! json nodes to work with
    type(json_value), pointer :: tmp_json_ptr
    type(json_file) :: my_file
    real(wp) :: raw_array(imx,jmx,kmx)             !! raw work array
    real(wp) :: array_element
    real(wp), allocatable :: fetched_array(:)
    character(kind=CK,len=:), allocatable :: description
    integer :: i,j,k                               !! loop indices
    integer :: array_length
    logical :: existed
    logical, allocatable :: SOS(:)

    error_cnt = 0
    call json_initialize(verbose=.true.,real_format='G')
    call check_errors()

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 12'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! populate the raw array
    forall (i=1:imx,j=1:jmx,k=1:kmx) ! could use size(... , dim=...) instead of constants
       raw_array(i,j,k) = i + (j-1)*imx + (k-1)*imx*jmx
    end forall

    call json_create_object(root,dir//file)
    call check_errors()

    call json_create_object(meta_array,'array data')
    call check_errors()

    shape = [size(raw_array,dim=1), size(raw_array,dim=2), size(raw_array,dim=3)]
    call json_add(meta_array, 'shape', shape)
    call check_errors()

    call json_add(meta_array, 'total size', size(raw_array))
    call check_errors()

    call json_update(meta_array, 'total size', size(raw_array), found=existed)
    call check_errors(existed)

    call json_add(meta_array, CK_'description', 'test data')
    call check_errors()

    ! now add the array
    ! N.B. `json_add()` only accepts 1-D arrays and scalars, so transform with `reshape`
    ! N.B. reshape populates new array in "array element order".
    ! C.F. "Modern Fortran Explained", by Metcalf, Cohen and Reid, p. 24.
    ! N.B. Fortran is a column major language

    call json_add( meta_array, 'data', reshape( raw_array, [ size(raw_array) ] ) )
    call check_errors()

    ! now put it all together
    call json_add(root,meta_array)
    call check_errors()

    write(error_unit,'(A)') "Print the JSON object to stderr:"
    call json_print(root,error_unit)
    call check_errors()

    call json_get(root,'$.array data.data(1)',array_element)
    call check_errors(abs(array_element - 1.0_wp) <= TOL)

    call json_get(root,'@.array data.shape',fetched_shape)
    call check_errors(all(fetched_shape == shape))

    call json_update(meta_array,'description',CK_'Test Data',found=existed)
    call check_errors(existed)

    call json_update(meta_array,CK_'description','Test data',found=existed)
    call check_errors(existed)

    call json_get(meta_array,'description',description)
    call check_errors('Test data' == description)

    call json_get(root,'array data.total size',array_length)
    call check_errors(array_length == imx*jmx*kmx)

    sos = [.true.,  .true.,  .true.,  &
           .false., .false., .false., &
           .true., .true., .true.]
    call json_add(root,'SOS',sos)
    call check_errors()

    call json_get(root,'SOS',sos)
    call check_errors()

    call json_add(root,'vector string', [CK_'only one value'])
    call check_errors()

    call json_add(root,CK_'page', ['The quick brown fox     ', 'jumps over the lazy dog.'])
    call check_errors()

    call json_get(root,'SOS',tmp_json_ptr)
    call check_errors()

    call json_get(tmp_json_ptr,sos)
    call check_errors()

    call json_get(meta_array,'shape',tmp_json_ptr)
    call check_errors()

    call json_get(tmp_json_ptr,fetched_shape)
    call check_errors(all(fetched_shape == shape))

    call json_get(meta_array,'data',tmp_json_ptr)
    call check_errors()

    call json_get(tmp_json_ptr,fetched_array)
    call check_errors(all(abs(fetched_array - reshape(raw_array,[size(raw_array)])) <= TOL))

    call json_get(root,'array data.data',fetched_array)
    call check_errors(all(abs(fetched_array - reshape(raw_array,[size(raw_array)])) <= TOL))

    raw_array = 0
    call json_get(me=root,path='array data.data',array_callback=get_3D_from_array)
    call check_errors(all(abs(fetched_array - reshape(raw_array,[size(raw_array)])) <= TOL))

    my_file = json_file(root)

    call my_file%update('array data.description',CK_'vector data',found=existed)
    call check_errors(existed)

    call my_file%update(CK_'array data.description','Vector data',found=existed)
    call check_errors(existed)

    call my_file%get('SOS',sos)
    call check_errors()

    call my_file%get('$array data.data',fetched_array)
    call check_errors(all(abs(fetched_array - reshape(raw_array,[size(raw_array)])) <= TOL))

    call my_file%get(tmp_json_ptr)
    call check_errors(associated(tmp_json_ptr,root))

    contains
      subroutine check_errors(assertion)
        logical, optional, intent(in) :: assertion
        if (json_failed()) then
           call json_print_error_message(error_unit)
           error_cnt = error_cnt + 1
        end if
        if (present (assertion)) then
           if (.not. assertion) error_cnt = error_cnt + 1
        end if
      end subroutine

      subroutine get_3D_from_array(element, i, count)
        type(json_value), pointer , intent(in)   :: element
        integer         , intent(in)             :: i        !!index
        integer         , intent(in)             :: count    !!size of array
        integer :: useless !! assign count to this to silence warnings

        ! let's pretend we're c programmers!
        call json_get( element, raw_array( &
             mod(i-1,imx) + 1, &            ! i index
             mod((i-1)/imx,jmx) + 1, &      ! j index
             mod((i-1)/imx/jmx,kmx) + 1 ) ) ! k inded
        useless = count
      end subroutine

    end subroutine

end module

program jf_test_12
    use jf_test_12_mod, only: test_12
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_12(n_errors)
    if ( n_errors /= 0) stop 1
end program
