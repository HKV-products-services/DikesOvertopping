!> @file
!! Main program for running the unit tests for probabilistic and many other functions
!<
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
!
! $Id$
!
program unitTestsProgram

    use ftnunit
    use utilities
    use feedback
    use feedback_parameters
    use overtoppingRTOTests

    use, intrinsic :: ieee_exceptions

    implicit none

    ! Enable following line to catch division by zero
    ! call ieee_set_halting_mode( IEEE_DIVIDE_BY_ZERO, .true. )

    call prepareTests

    call runtests_init
    call setRunTestLevel(3) ! should be 3

    call runtests(allovertoppingRTOTests)

    call runtests_final
    call showResult

contains

!> Routine to be called when tests are started
subroutine initTestProgram
    ! Set action for feedback when a fatal error occurs
    implicit none
    call SetFatalerrorAction ( onfatalerrorUnittest )
end subroutine initTestProgram

!> Routine to be called when single tests are started
subroutine initTest
    implicit none
    ! reset feedback flags
    call SetFatalErrorExpected ( .false.)
    call ResetFatalErrorOccured()
end subroutine initTest

!> Routine to start the testing
!! Note:
!!     This routine merely takes care that the unit tests
!!     are indeed run
subroutine prepareTests
    implicit none

    integer  :: lun   !< LU-number
    
    ! Initialization called from ftnunit before running tests
    subptrTestProgramInit => initTestProgram
    
    ! Initialization called from ftnunit before running each separate test
    subptrTestInit => initTest
    
    call getFreeLuNumber( lun )
    open( lun, file = 'ftnunit.run' )
    write( lun, '(a)' ) 'ALL'
    close( lun )

end subroutine prepareTests

!> Start the browser to show the result
!!
subroutine showResult
    implicit none
    !character(len=1) :: answer
    !
    !write(*,*)     'Press ENTER ...'
    !read(*,'(a)' ) answer

    call system( 'ftnunit.html' )

end subroutine showResult

end program unitTestsProgram
