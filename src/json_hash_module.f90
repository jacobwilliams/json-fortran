!*****************************************************************************************
!>
!  A simple hash table implementation in Fortran using separate chaining for collision handling.
!
!  This module defines a hash table type with methods for initialization, insertion, lookup, and
!  deletion. It uses the DJB hash algorithm for hashing string keys and supports optional
!  case-insensitive key comparison.

module json_hash_module
    use json_kinds,            only: RK, IK
    use json_value_module,     only: json_value, json_core
    use json_string_utilities, only: lowercase_string
    use json_parameters,       only: json_object

    implicit none

    private

    type :: hash_node_t
        !! This is a linked list to avoid collisions in the hash table buckets.
        private
        character(len=:), allocatable :: key !! string key. if `case_sensitive_keys=False` then
                                             !! this will always be lowercase.
        type(json_value), pointer :: value => null() !! associated value in the hash table
        type(hash_node_t), pointer :: next => null()
    end type hash_node_t

    type, public :: json_hash_table
        !! Hash table type.
        !!
        !! The keys are the member names of a JSON object and the
        !! values are pointers to the associated JSON values.
        !!
        !! This can be used to speed up lookups of JSON object members by name,
        !! since the JSON library itself uses linked lists to store the members
        !! of a JSON object, which can lead to `O(n)` lookup time for `n` members.
        !! By creating a hash table for the members of a JSON object,
        !! we can achieve `O(1)` average case lookup time.

        private

        type(hash_node_t), dimension(:), allocatable :: buckets
        integer(IK) :: size = 0_IK            !! Number of elements in the table
        integer(IK) :: capacity = 16_IK       !! Number of buckets in the table
        real(RK) :: load_factor = 0.75_RK     !! Resize threshold that determines when the
                                              !! hash table should automatically resize to
                                              !! maintain optimal performance.
        logical :: case_sensitive_keys = .true. !! Case-sensitive key comparison
        logical :: trailing_spaces_significant = .false. !! Whether trailing spaces in keys
                                                         !! are significant for comparison

        contains

        private

        procedure,public :: create  => hash_table_create
        procedure,public :: destroy => hash_table_destroy
        procedure,public :: get     => hash_table_get

        procedure :: init => hash_table_init
        procedure :: insert => hash_table_insert
        procedure :: hash => hash_table_hash
        procedure :: resize => hash_table_resize
        procedure :: preprocess_key
        procedure :: keys_equal
    end type json_hash_table

contains
!*****************************************************************************************

    !*******************************************************************************
    !>
    !  Initialize hash table with optional initial capacity

    subroutine hash_table_init(me, json, initial_capacity)

        class(json_hash_table), intent(inout) :: me
        type(json_core), intent(in) :: json
        integer(IK), intent(in), optional :: initial_capacity

        integer :: i !! counter

        if (present(initial_capacity)) then
            me%capacity = initial_capacity
        end if

        ! get the key settings from the JSON object so that the hash
        ! table will be consistent with how keys are compared in the JSON library.
        call json%get_name_settings(case_sensitive_keys = me%case_sensitive_keys,&
                                    trailing_spaces_significant = me%trailing_spaces_significant)

        allocate(me%buckets(0:me%capacity-1))

        ! Initialize each bucket head node
        do i = 0, me%capacity - 1
            me%buckets(i)%next => null()
        end do

        me%size = 0
    end subroutine hash_table_init

    !*******************************************************************************
    !>
    !  Destroy hash table and free all memory

    subroutine hash_table_destroy(me)
        class(json_hash_table), intent(inout) :: me

        integer :: i !! counter
        type(hash_node_t), pointer :: current, temp

        if (.not. allocated(me%buckets)) return

        ! Free all linked lists
        do i = 0, me%capacity - 1
            current => me%buckets(i)%next
            do while (associated(current))
                temp => current
                current => current%next
                if (allocated(temp%key)) deallocate(temp%key)
                deallocate(temp)
            end do
        end do

        deallocate(me%buckets)
        me%size = 0_IK
    end subroutine hash_table_destroy

    !*******************************************************************************
    !>
    !  DJB hash algorithm for a string.
    !
    !### See also
    !  * J. Shahbazian, Fortran hashing algorithm, July 6, 2013
    !   [Fortran Dev](https://fortrandev.wordpress.com/2013/07/06/fortran-hashing-algorithm/)

    function hash_table_hash(me, key) result(hash_value)
        class(json_hash_table), intent(in) :: me
        character(len=*), intent(in) :: key
        integer(kind=IK) :: hash_value

        integer(kind=IK) :: hash
        integer :: i !! counter

        hash = 5381_IK
        do i = 1, len_trim(key)
            hash = ishft(hash, 5_IK) + hash + iachar(key(i:i), kind=IK)  ! hash * 33 + c
        end do

        ! Map to bucket index
        hash_value = int(modulo(hash, int(me%capacity, IK)), IK)
    end function hash_table_hash

    !*******************************************************************************
    !>
    !  Preprocess key for saving in the hash table, as well as for
    !  insertion and lookup (handle trim and case-sensitivity).

    function preprocess_key(me, key) result(proc_key)
        class(json_hash_table), intent(in) :: me
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: proc_key

        ! if spaces are not significant, trim the key.
        if (me%trailing_spaces_significant) then
            proc_key = key
        else
            proc_key = trim(key)
        end if
        !if case-insensitive, lowercase the key.
        if (.not. me%case_sensitive_keys) proc_key = lowercase_string(proc_key)
    end function preprocess_key

    !*******************************************************************************
    !>
    !  Compare two keys for equality, taking into account the setting
    !  for trailing spaces.
    !
    !  Is it assumed that [[preprocess_key]] has already been applied
    !  to both keys before calling this function. This allows us to
    !  avoid redundant lowercasing and trimming of the keys.
    !
    !### See also
    !  * [[json_core(type):name_equal]]

    function keys_equal(me, key1, key2) result(equal)

        class(json_hash_table), intent(in) :: me
        character(len=*), intent(in) :: key1, key2
        logical :: equal

        if (me%trailing_spaces_significant) then
            ! if trailing spaces are significant, then the keys
            ! must be equal in length
            equal = len(key1) == len(key2)
            if (.not. equal) return
        end if
        equal = key1 == key2

    end function keys_equal

    !*******************************************************************************
    !>
    !  Create a hash table from the children of an existing JSON object.
    !  The keys are the object member names and the values are
    !  pointers to the associated JSON values.

    subroutine hash_table_create(me, json, p, status_ok)

        class(json_hash_table), intent(out) :: me
        type(json_core), intent(inout) :: json
        type(json_value), pointer :: p !! the JSON value whose children will be used to populate
                                       !! the hash table. This must be a JSON object.
        logical, intent(out) :: status_ok !! true if no problems.

        integer(IK) :: i !! counter
        integer(IK) :: n_members !! number of members in the JSON object
        integer (IK) :: var_type !! type of the JSON value
        character(len=:), allocatable :: key !! member names
        type(json_value), pointer :: current !! pointer to current member value
        type(json_value), pointer :: next !! pointer to next member value

        if (.not. associated(p)) then
            status_ok = .false.
            return
        end if

        status_ok = .true.
        call json%info(p, n_children=n_members, var_type=var_type)
        if (n_members > 0_IK .and. var_type == json_object) then
            ! size it initally to the number of members divided by the load factor to minimize
            ! the number of resizes needed as members are inserted.
            call me%init(json, initial_capacity=ceiling(real(n_members, RK) / me%load_factor))
            ! iterate over each child member and insert into the hash table
            call json%get_child(p, 1, current) ! the first one
            do i = 1, n_members
                call json%info(current, name=key)
                call me%insert(key, current)
                call json%get_next(current, next) ! get the next one in the list of children
                current => next
            end do
        else
            ! can't create a hash table.
            status_ok = .false.
        end if

    end subroutine hash_table_create
    !*******************************************************************************

    !*******************************************************************************
    !>
    !  Insert or update a key-value pair

    subroutine hash_table_insert(me, key, value)
        class(json_hash_table), intent(inout) :: me
        character(len=*), intent(in) :: key
        type(json_value),pointer :: value

        integer(IK) :: bucket_idx
        type(hash_node_t), pointer :: current, new_node
        real(RK) :: current_load
        character(len=:), allocatable :: key_to_insert

        ! Check if we need to resize
        current_load = real(me%size, RK) / real(me%capacity, RK)
        if (current_load >= me%load_factor) then
            call me%resize(me%capacity * 2_IK)
        end if

        ! preprocess key for insertion (case-insensitive if enabled)
        key_to_insert = me%preprocess_key(key)

        bucket_idx = me%hash(key_to_insert)
        current => me%buckets(bucket_idx)%next

        ! Check if key already exists
        do while (associated(current))
            if (me%keys_equal(current%key, key_to_insert)) then
                ! Update existing value
                current%value => value
                return
            end if
            current => current%next
        end do

        ! Insert new node at the beginning
        allocate(new_node)
        new_node%key = key_to_insert
        new_node%value => value
        new_node%next => me%buckets(bucket_idx)%next
        me%buckets(bucket_idx)%next => new_node
        me%size = me%size + 1
    end subroutine hash_table_insert

    !*******************************************************************************
    !>
    !  Get value for a given key

    function hash_table_get(me, key, found) result(value)
        class(json_hash_table), intent(in) :: me
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        type(json_value), pointer :: value

        integer(IK) :: bucket_idx
        type(hash_node_t), pointer :: current
        character(len=:), allocatable :: key_to_search

        value => null()
        if (present(found)) found = .false.

        if (.not. allocated(me%buckets)) return

        key_to_search = me%preprocess_key(key)

        bucket_idx = me%hash(key_to_search)
        current => me%buckets(bucket_idx)%next

        do while (associated(current))
            if (me%keys_equal(current%key, key_to_search)) then
                value => current%value
                if (present(found)) found = .true.
                return
            end if
            current => current%next
        end do
    end function hash_table_get

    !*******************************************************************************
    !>
    !  Resize the hash table (internal method)

    subroutine hash_table_resize(me, new_capacity)

        class(json_hash_table), intent(inout) :: me
        integer, intent(in) :: new_capacity

        type(hash_node_t), dimension(:), allocatable :: old_buckets
        type(hash_node_t), pointer :: current, temp
        integer :: i, old_capacity

        ! Save old buckets
        old_capacity = me%capacity
        call move_alloc(me%buckets, old_buckets)

        ! Allocate new buckets
        me%capacity = new_capacity
        allocate(me%buckets(0:me%capacity-1))
        do i = 0, me%capacity - 1
            me%buckets(i)%next => null()
        end do
        me%size = 0

        ! Rehash all elements
        do i = 0, old_capacity - 1
            current => old_buckets(i)%next
            do while (associated(current))
                call me%insert(current%key, current%value)
                temp => current
                current => current%next
                if (allocated(temp%key)) deallocate(temp%key)
                deallocate(temp)
            end do
        end do

        deallocate(old_buckets)
    end subroutine hash_table_resize

!*****************************************************************************************
    end module json_hash_module
!*****************************************************************************************