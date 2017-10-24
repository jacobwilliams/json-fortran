
!****************************************************************************
!
!  PROGRAM: jsonfortrantest
!
!  PURPOSE:  Entry point for the console application.
!
!            runs through all the tests in the tests folder returns 1 on shut down if there are any errors.    
!
!****************************************************************************

program jsonfortrantest

    use jf_test_1_mod , only: test_1
    use jf_test_2_mod , only: test_2
    use jf_test_3_mod , only: test_3
    use jf_test_4_mod , only: test_4
    use jf_test_5_mod , only: test_5
    use jf_test_6_mod , only: test_6
    use jf_test_7_mod , only: test_7
    use jf_test_8_mod , only: test_8
    use jf_test_9_mod , only: test_9
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
    use jf_test_29_mod , only: test_29   
    
    
    implicit none
    integer :: n_errors

    call test_1(n_errors)
    call test_2(n_errors)
    call test_3(n_errors)
    call test_4(n_errors)
    call test_5(n_errors)
    call test_6(n_errors)
    call test_7(n_errors)
    call test_8(n_errors)
    call test_9(n_errors)
    call test_10(n_errors)
    call test_11(n_errors)
    call test_12(n_errors)
    call test_13(n_errors)
    call test_14(n_errors)
    call test_15(n_errors)
    call test_16(n_errors)
    call test_17(n_errors)
    call test_18(n_errors)
    call test_19(n_errors)
    call test_20(n_errors)
    call test_21(n_errors)
    call test_22(n_errors)
    call test_23(n_errors)
    call test_24(n_errors)
    call test_25(n_errors)
    call test_26(n_errors)
    call test_27(n_errors)
    call test_29(n_errors)
    
    if (n_errors /= 0) stop 1
    
end program jsonfortrantest


