! Copyright (C) Stichting Deltares 2022. All rights reserved.
!
! This file is part of the Dikes Overtopping Kernel.
!
! The Dikes Overtopping Kernel is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with this program. If not, see <http://www.gnu.org/licenses/>.
!
! All names, logos, and references to "Deltares" are registered trademarks of
! Stichting Deltares and remain full property of Stichting Deltares at all times.
! All rights reserved.
!

!> @file
!! Main program for running the unit tests for dikes overtopping
!<
!
! $Id$
!
program unitTestsProgram
#if defined _WIN32 || defined _WIN64
    use user32
    use kernel32
#endif
    use precision, only : pntlen
    use ftnunit
    use overtoppingTests

    use, intrinsic :: ieee_exceptions

    implicit none
    character(len=16) :: version
#if defined _WIN32 || defined _WIN64
    pointer            (qv, versionNumber)
    integer(kind=pntlen)           :: p
#endif

    ! Enable following line to catch division by zero
    ! call ieee_set_halting_mode( IEEE_DIVIDE_BY_ZERO, .true. )
#if defined _WIN32 || defined _WIN64
    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qv = getprocaddress (p, "versionNumber"C)

    call versionNumber(version)
    call setTestTitle("Unit tests DikesOvertopping " // merge("64-bit", "32-bit", pntlen==8) // ", version: " // trim(version) )
#else
    call setTestTitle("Unit tests DikesOvertopping Linux version")
#endif

    call prepareTests

    call runtests_init

    call setRunTestLevel(3) ! should be 3

    call runtests(allOvertoppingTests)

    call runtests_final
    call showResult

contains


!> Routine to be called when tests are started
subroutine initTestProgram
    ! Set action for feedback when a fatal error occurs
    implicit none
    continue ! intended empty subroutine
end subroutine initTestProgram

!> Routine to be called when single tests are started
subroutine initTest
    implicit none
    continue ! intended empty subroutine
end subroutine initTest

!> Routine to start the testing
!! Note:
!!     This routine merely takes care that the unit tests
!!     are indeed run
subroutine prepareTests
    implicit none

    integer  :: lun   !< LU-number
    
    ! Initialization called from ftnunit before running tests
    ! disabled, as it fails on Windows-10, and on Windows-7 it calls an empty subroutine
    !subptrTestProgramInit => initTestProgram
    
    ! Initialization called from ftnunit before running each separate test
    ! disabled, as it fails on Windows-10, and on Windows-7 it calls an empty subroutine
    !subptrTestInit => initTest

    open( newunit=lun, file = 'ftnunit.run' )
    write( lun, '(a)' ) 'ALL'
    close( lun )

end subroutine prepareTests

!> Start the browser to show the result
!!
subroutine showResult
    implicit none

    call system( 'ftnunit.html' )

end subroutine showResult

end program unitTestsProgram
