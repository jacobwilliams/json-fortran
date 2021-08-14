!*****************************************************************************************
!>
!  Entry point for the unified unit test application.
!
!  Runs all the tests in the `tests` folder
!  Returns `1` if there are any errors.

    program jsonfortrantest

    use jf_test_1_mod  , only: test_1
    use jf_test_2_mod  , only: test_2
    use jf_test_3_mod  , only: test_3
    use jf_test_4_mod  , only: test_4
    use jf_test_5_mod  , only: test_5
    use jf_test_6_mod  , only: test_6
    use jf_test_7_mod  , only: test_7
    use jf_test_8_mod  , only: test_8
    use jf_test_9_mod  , only: test_9
    use jf_test_10_mod , only: test_10
    use jf_test_11_mod , only: test_11
    use jf_test_12_mod , only: test_12
    use jf_test_13_mod , only: test_13
    use jf_test_14_mod , only: test_14
    use jf_test_15_mod , only: test_15
    use jf_test_16_mod , only: test_16
    use jf_test_17_mod , only: test_17
    use jf_test_18_mod , only: test_18
    use jf_test_19_mod , only: test_19
    use jf_test_20_mod , only: test_20
    use jf_test_21_mod , only: test_21
    use jf_test_22_mod , only: test_22
    use jf_test_23_mod , only: test_23
    use jf_test_24_mod , only: test_24
    use jf_test_25_mod , only: test_25
    use jf_test_26_mod , only: test_26
    use jf_test_27_mod , only: test_27
    use jf_test_28_mod , only: test_28
    use jf_test_29_mod , only: test_29
    use jf_test_30_mod , only: test_30
    use jf_test_31_mod , only: test_31
    use jf_test_32_mod , only: test_32
    use jf_test_33_mod , only: test_33
    use jf_test_34_mod , only: test_34
    use jf_test_35_mod , only: test_35
    use jf_test_36_mod , only: test_36
    use jf_test_37_mod , only: test_37
    use jf_test_38_mod , only: test_38
    use jf_test_39_mod , only: test_39
    use jf_test_40_mod , only: test_40
    use jf_test_41_mod , only: test_41
    use jf_test_42_mod , only: test_42
    use jf_test_43_mod , only: test_43
    use jf_test_44_mod , only: test_44
    use jf_test_45_mod , only: test_45
    use jf_test_46_mod , only: test_46
    use jf_test_47_mod , only: test_47
    use jf_test_48_mod , only: test_48
    use jf_test_49_mod , only: test_49

    implicit none

    integer :: n_errors  !! number of errors

    n_errors = 0

    call test_1 (n_errors); if (n_errors /= 0) stop 1
    call test_2 (n_errors); if (n_errors /= 0) stop 1
    call test_3 (n_errors); if (n_errors /= 0) stop 1
    call test_4 (n_errors); if (n_errors /= 0) stop 1
    call test_5 (n_errors); if (n_errors /= 0) stop 1
    call test_6 (n_errors); if (n_errors /= 0) stop 1
    call test_7 (n_errors); if (n_errors /= 0) stop 1
    call test_8 (n_errors); if (n_errors /= 0) stop 1
    call test_9 (n_errors); if (n_errors /= 0) stop 1
    call test_10(n_errors); if (n_errors /= 0) stop 1
    call test_11(n_errors); if (n_errors /= 0) stop 1
    call test_12(n_errors); if (n_errors /= 0) stop 1
    call test_13(n_errors); if (n_errors /= 0) stop 1
    call test_14(n_errors); if (n_errors /= 0) stop 1
    call test_15(n_errors); if (n_errors /= 0) stop 1
    call test_16(n_errors); if (n_errors /= 0) stop 1
    call test_17(n_errors); if (n_errors /= 0) stop 1
    call test_18(n_errors); if (n_errors /= 0) stop 1
    call test_19(n_errors); if (n_errors /= 0) stop 1
    call test_20(n_errors); if (n_errors /= 0) stop 1
    call test_21(n_errors); if (n_errors /= 0) stop 1
    call test_22(n_errors); if (n_errors /= 0) stop 1
    call test_23(n_errors); if (n_errors /= 0) stop 1
    call test_24(n_errors); if (n_errors /= 0) stop 1
    call test_25(n_errors); if (n_errors /= 0) stop 1
    call test_26(n_errors); if (n_errors /= 0) stop 1
    call test_27(n_errors); if (n_errors /= 0) stop 1
    call test_28(n_errors); if (n_errors /= 0) stop 1
    call test_29(n_errors); if (n_errors /= 0) stop 1
    call test_30(n_errors); if (n_errors /= 0) stop 1
    call test_31(n_errors); if (n_errors /= 0) stop 1
    call test_32(n_errors); if (n_errors /= 0) stop 1
    call test_33(n_errors); if (n_errors /= 0) stop 1
    call test_34(n_errors); if (n_errors /= 0) stop 1
    call test_35(n_errors); if (n_errors /= 0) stop 1
    call test_36(n_errors); if (n_errors /= 0) stop 1
    call test_37(n_errors); if (n_errors /= 0) stop 1
    call test_38(n_errors); if (n_errors /= 0) stop 1
    call test_39(n_errors); if (n_errors /= 0) stop 1
    call test_40(n_errors); if (n_errors /= 0) stop 1
    call test_41(n_errors); if (n_errors /= 0) stop 1
    call test_42(n_errors); if (n_errors /= 0) stop 1
    call test_43(n_errors); if (n_errors /= 0) stop 1
    call test_44(n_errors); if (n_errors /= 0) stop 1
    call test_45(n_errors); if (n_errors /= 0) stop 1
    call test_46(n_errors); if (n_errors /= 0) stop 1
    call test_47(n_errors); if (n_errors /= 0) stop 1
    call test_48(n_errors); if (n_errors /= 0) stop 1
    call test_49(n_errors); if (n_errors /= 0) stop 1

    end program jsonfortrantest
!*****************************************************************************************

