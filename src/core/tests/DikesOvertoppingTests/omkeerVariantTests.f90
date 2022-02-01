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
!! Contains the module omkeerVariantTests of the DikesOvertopping dll
!
! $Id$
!
!> 
!! Module, holding tests of the 'omkeerVariant' functions within the DikesOvertopping dll.
!!
!! @ingroup DikeOvertoppingTests
module omkeerVariantTests
use precision, only : wp, pntlen
use typeDefinitionsOvertopping
use overtoppingInterface
use ModuleLogging
use errorMessages, only : tMessage
use ftnunit
use testHelper, only : init_modelfactors_and_load
use omkeerVariantModule
#if defined _WIN32 || defined _WIN64
use user32
use kernel32
#endif

implicit none

private

public :: allOmkeerVariantTests

contains

!> Performs all tests
subroutine allOmkeerVariantTests
    call testWithLevel(omkeerVariantTestBasic,                                "OmkeerVariant; inverse of overtoppingDllTest test", 1)
    call testWithLevel(omkeerVariantTestWithHighDischarge,                    "OmkeerVariant; high discharge", 1)
    call testWithLevel(omkeerVariantTestWithDikeHeigthInProfile,              "OmkeerVariant; expected dikeheight in profile", 1)
    call testWithLevel(omkeerVariantTestWithBerm,                             "OmkeerVariant; with berm", 1)
    call testWithLevel(omkeerVariantTestWithBermAndDikeHeightJustAboveBerm1,  "OmkeerVariant; with berm and dikeheight just above berm", 1)
    call testWithLevel(omkeerVariantTestWithBermAndDikeHeightJustAboveBerm2,  "OmkeerVariant; with berm and expected dikeheight just above berm", 1)
    call testWithLevel(omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm, "OmkeerVariant; with 1:15 berm and expected dikeheight halfway berm", 1)
    call testWithLevel(omkeerVariantTestWithVerySmallDischarge,               "OmkeerVariant; with a very small discharge", 1)
    call testWithLevel(omkeerVariantTestWithWaterlevelBelowToe,               "OmkeerVariant; with water level below toe", 1)
    call testWithLevel(omkeerVariantTestWithExpectedDikeheightHalfwaySlope,   "OmkeerVariant; with expected dikeheight halfway last slope segment", 1)
    call testWithLevel(omkeerVariantIssue34A,                                 "OmkeerVariant; ISSUE; test A related to issue 34", 1)
    call testWithLevel(omkeerVariantIssue34B,                                 "OmkeerVariant; ISSUE; test B related to issue 34", 1)
    call testWithLevel(omkeerVariantIssue34C,                                 "OmkeerVariant; ISSUE; test C related to issue 34", 1)
    call testWithLevel(omkeerVariantIssue35A,                                 "OmkeerVariant; ISSUE; test A related to issue 35", 1)
    call testWithLevel(omkeerVariantIssue35B,                                 "OmkeerVariant; ISSUE; test B related to issue 35", 1)
    call testWithLevel(omkeerVariantIssue35C,                                 "OmkeerVariant; ISSUE; test C related to issue 35", 1)
    call testWithLevel(omkeerVariantIssue36A,                                 "OmkeerVariant; ISSUE; test A related to issue 36: wl at toe; 1:4; long dike", 1)
    call testWithLevel(omkeerVariantIssue36B,                                 "OmkeerVariant; ISSUE; test B related to issue 36: wl at toe; 1:4; short dike", 1)
    call testWithLevel(omkeerVariantIssue42A,                                 "OmkeerVariant; ISSUE; test A related to issue 42: berm at waterlevel", 1)
    call testWithLevel(omkeerVariantIssue42B,                                 "OmkeerVariant; ISSUE; test B related to issue 42: resulting dikeheight at end of berm", 1)
    call testWithLevel(omkeerVariantIssue51A,                                 "OmkeerVariant; ISSUE; test A related to issue 51", 1)
    call testWithLevel(omkeerVariantLoadTests,                                "OmkeerVariant; load tests", 1)
    call testWithLevel(omkeerVariantIssue52,                                  "OmkeerVariant; ISSUE; test A related to issue 52", 1)
end subroutine allOmkeerVariantTests

!> inverse of overtoppingDllTest test:
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestBasic
    integer(kind=pntlen)           :: p
    external                       :: omkeerVariant
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

    pointer            (q, omkeerVariant)

#if defined _WIN32 || defined _WIN64
    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    call assert_true(p /= 0, 'load dllDikesOvertopping.dll')
    q = getprocaddress (p, "omkeerVariantF"C)
    call assert_true(q /= 0, 'get function pointer to omkeerVariant')
#endif
    if (q == 0) return
    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in omkeerVariant for waterlevel < dikeheight
    !
    givenDischarge = 0.8d-8
    call omkeerVariant(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_comparable(dikeHeight, 9.1d0, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1.5d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestBasic

!> as omkeerVariantTestBasic, but not through dll and max. discharge 
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithHighDischarge
    integer                        :: i
    integer, parameter             :: npoints = 3
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 0.2d0
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 5.522_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithHighDischarge

!> as omkeerVariantTestWithHighDischarge, but with a expected discharge near y(2)
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithDikeHeigthInProfile
    integer                        :: i
    integer, parameter             :: npoints = 3
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 1.568D-4  ! exact : 1.565674960996936D-004
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 7.025_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithDikeHeigthInProfile

!> as omkeerVariantTestWithHighDischarge, but with berm
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithBerm
    integer                        :: i, ii
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if (i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 0.2d0
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 5.522_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithBerm

!> as omkeerVariantTestWithBerm, but with dikeheight just above berm
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithBermAndDikeHeightJustAboveBerm1
    integer                        :: i, ii
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if (i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%ycoords(npoints) = geometryF%ycoords(npoints-1) + .5_wp
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 0.2d0
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 5.522_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithBermAndDikeHeightJustAboveBerm1

!> as omkeerVariantTestWithBermAndDikeHeightJustAboveBerm1, but with expected dikeheight just above berm
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithBermAndDikeHeightJustAboveBerm2
    integer                        :: i
    integer                        :: ii
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if (i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%ycoords(npoints) = geometryF%ycoords(npoints-1) + .5_wp
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 1.763396051957844D-004
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 7.000_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithBermAndDikeHeightJustAboveBerm2

!> as omkeerVariantTestWithBermAndDikeHeightJustAboveBerm2, but with 1:15 berm, and expected dikeheight halfway berm
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm
    integer                        :: i, ii
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if (i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    bermLength = geometryF%xcoords(3) - geometryF%xcoords(2)
    dy = bermLength / 15d0
    geometryF%ycoords(3) = geometryF%ycoords(2) + dy
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 8.005817025697209D-5
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 7.166_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm

!> as omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm, but with a very low discharge and consequently a very high dikeheight
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithVerySmallDischarge
    integer                        :: i, ii
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if (i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    bermLength = geometryF%xcoords(3) - geometryF%xcoords(2)
    dy = bermLength / 15d0
    geometryF%ycoords(3) = geometryF%ycoords(2) + dy
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 1d-20
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 14.86_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithVerySmallDischarge

!> as omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm, test water level below toe
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithWaterlevelBelowToe
    integer                        :: i
    integer                        :: ii
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load, 4.90_wp)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if (i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    bermLength = geometryF%xcoords(3) - geometryF%xcoords(2)
    dy = bermLength / 15d0
    geometryF%ycoords(3) = geometryF%ycoords(2) + dy
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < toe
    !
    givenDischarge = 2d-4
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, load%h, 1d-15, 'dikeHeight from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithWaterlevelBelowToe

!> as omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm, but expected dikeheight halfway last slope segment
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithExpectedDikeheightHalfwaySlope
    integer                        :: i
    integer                        :: ii
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if (i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    bermLength = geometryF%xcoords(3) - geometryF%xcoords(2)
    dy = bermLength / 15d0
    geometryF%ycoords(3) = geometryF%ycoords(2) + dy
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheight
    !
    givenDischarge = 3d-6
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 7.856_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithExpectedDikeheightHalfwaySlope

!> test for issue Overs-34
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue34A
    integer                        :: i
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)
    load%h        =  6.21_wp
    load%phi      = 90.00_wp
    load%Hm0      =  0.20_wp
    load%Tm_10    =  1.60_wp

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 18.6_wp * real(i-1, wp)
        geometryF%ycoords(i)   = geometryF%xcoords(i) / 3.0_wp
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 90.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge
    !
    givenDischarge = 1D-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, load%h + 0.269, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.492_wp, 0.001_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue34A

!> test for issue Overs-34
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue34B
    integer                        :: i
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)
    load%h        =  6.00_wp
    load%phi      = 90.00_wp
    load%Hm0      =  0.20_wp
    load%Tm_10    =  1.60_wp

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 300.0_wp * real(i-1,wp)
        geometryF%ycoords(i)   = geometryF%xcoords(i) / 3.0_wp
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 90.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge
    !
    givenDischarge = 1D-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, load%h + 0.269, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.492_wp, 0.001_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue34B

!> test for issue Overs-34
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue34C
    integer                        :: i
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)
    load%h        =  0.50_wp
    load%phi      = 90.00_wp
    load%Hm0      =  0.016_wp
    load%Tm_10    =  1.20_wp

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 18.6_wp * real(i-1,wp)
        geometryF%ycoords(i)   = geometryF%xcoords(i) / 3.0_wp
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 90.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge
    !
    givenDischarge = 1D-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 0.50165_wp, 1d-4, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.0519253_wp, 0.0001_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue34C

!> test for issue Overs-35: no waves
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue35A
    type (tpLoad)                  :: load              !< structure with load data

    !
    ! initializations
    !
    load%h        =  0.50_wp
    load%phi      = 110.00_wp
    load%Hm0      =  0.16_wp
    load%Tm_10    =  1.20_wp

    call omkeerVariantIssue35(load, 0.0_wp)

end subroutine omkeerVariantIssue35A

!> test for issue Overs-35: no waves
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue35B
    type (tpLoad)                  :: load              !< structure with load data

    !
    ! initializations
    !
    load%h        =  0.50_wp
    load%phi      = 90.00_wp
    load%Hm0      =  0.0_wp
    load%Tm_10    =  1.20_wp

    call omkeerVariantIssue35(load, 90.0_wp)

end subroutine omkeerVariantIssue35B

!> test for issue Overs-35: no waves
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue35C
    type (tpLoad)                  :: load              !< structure with load data

    !
    ! initializations
    !
    load%h        =  0.50_wp
    load%phi      = 90.00_wp
    load%Hm0      =  0.0_wp
    load%Tm_10    =  1.20_wp

    call omkeerVariantIssue35(load, 90.0_wp)

end subroutine omkeerVariantIssue35C

!> test for issue Overs-35: no waves
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue35(load, normal)
    type (tpLoad), intent(in)      :: load              !< structure with load data
    real(kind=wp), intent(in)      :: normal

    integer                        :: i
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 18.6_wp * real(i-1,wp)
        geometryF%ycoords(i)   = geometryF%xcoords(i) / 3.0_wp
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = normal ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge; no waves
    !
    givenDischarge = 1D-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, load%h, 1d-4, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.0_wp, 0.0001_wp, 'z2 from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue35

!> test for issue Overs-36: ...
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue36A
    type (tpLoad)                  :: load              !< structure with load data
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    !Teen op (0; 8)
    !Kruin op (368; 100)
    geometryF%xcoords      = [0.0_wp, 368.0_wp]
    geometryF%ycoords      = [8.0_wp, 100.0_wp]
    geometryF%roughness(1) = 1.0_wp
    geometryF%normal = 130.0_wp ! degrees
    geometryF%npoints = npoints
    !
    load%h        =  8.55_wp
    load%phi      = 45.00_wp
    load%Hm0      =  1.0_wp
    load%Tm_10    =  3.50_wp
    !
    ! test actual computations in iterateToGivenDischarge; ...
    !
    givenDischarge = 5d-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 8.97_wp, 0.01_wp, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.92_wp, 0.01_wp, 'z2 from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue36A

!> test for issue Overs-36: wl at toe; 1:4; short dike
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue36B
    type (tpLoad)                  :: load              !< structure with load data
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    !Teen op (0; 8)
    !Kruin op (368; 100)
    geometryF%xcoords      = [0.0_wp, 2.0_wp]
    geometryF%ycoords      = [8.0_wp, 8.5_wp]
    geometryF%roughness(1) = 1.0_wp
    geometryF%normal = 130.0_wp ! degrees
    geometryF%npoints = npoints
    !
    load%h        =  8.55_wp
    load%phi      = 45.00_wp
    load%Hm0      =  1.0_wp
    load%Tm_10    =  3.50_wp
    !
    ! test actual computations in iterateToGivenDischarge; ...
    !
    givenDischarge = 5d-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 8.97_wp, 0.01_wp, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.92_wp, 0.01_wp, 'z2 from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue36B

!> test for issue 42: berm at waterlevel
subroutine omkeerVariantIssue42A
    type (tpLoad)                  :: load              !< structure with load data
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    !
    geometryF%xcoords      = [0.0_wp, 24.0_wp, 29.0_wp, 45.0_wp]
    geometryF%ycoords      = [-1.0_wp, 5.0_wp, 5.0_wp, 9.0_wp]
    geometryF%roughness    = 1.0_wp
    geometryF%normal = 245.0_wp ! degrees
    geometryF%npoints = npoints
    !
    load%h        =  5.0_wp
    load%phi      = 240.00_wp
    load%Hm0      =  0.04_wp
    load%Tm_10    =  0.2_wp
    !
    ! test actual computations in iterateToGivenDischarge; ...
    !
    givenDischarge = 1d-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 5.0001_wp, 1e-4_wp, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.020386_wp, 1e-4_wp, 'z2 from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue42A

!> 2nd test for issue 42: resulting dikeheight at end of berm
subroutine omkeerVariantIssue42B
    type (tpLoad)                  :: load              !< structure with load data
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    !
    geometryF%xcoords      = [0.0_wp, 20.52_wp, 33.52_wp, 177.0_wp]
    geometryF%ycoords      = [0.0_wp, 5.13_wp, 5.13_wp, 53.0_wp]
    geometryF%roughness    = 1.0_wp
    geometryF%normal = 8.0_wp ! degrees
    geometryF%npoints = npoints
    !
    load%h        =  4.0_wp
    load%phi      = 304.45_wp
    load%Hm0      =  1.5345_wp
    load%Tm_10    =  2.5586_wp
    !
    ! test actual computations in iterateToGivenDischarge; ...
    !
    givenDischarge = 1d-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 5.13584_wp, 1e-4_wp, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.405_wp, 1e-4_wp, 'z2 from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue42B

subroutine omkeerVariantIssue51A
    type (tpLoad)                  :: load              !< structure with load data
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    !
    geometryF%xcoords      = [0.0_wp, 3.6_wp, 7.6_wp, 12.2_wp]
    geometryF%ycoords      = [0.0_wp, 1.8_wp, 1.9_wp,  4.2_wp]
    geometryF%roughness    = 1.0_wp
    geometryF%normal = 330.0_wp ! degrees
    geometryF%npoints = npoints
    !
    load%h        =  1.88_wp
    load%phi      = 292.5_wp
    load%Hm0      =  0.15_wp
    load%Tm_10    =  1.3_wp
    !
    ! test actual computations in iterateToGivenDischarge; ...
    !
    givenDischarge = 1d-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 1.88_wp, 1e-4_wp, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 0.0_wp, 1e-4_wp, 'z2 from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue51A

subroutine omkeerVariantLoadTests
    type (tpLoad)                  :: load              !< structure with load data
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    !
    geometryF%xcoords      = [-16.0_wp, 16.0_wp]
    geometryF%ycoords      = [ -4.0_wp,  4.0_wp]
    geometryF%roughness    = 1.0_wp
    geometryF%normal = 270.0_wp ! degrees
    geometryF%npoints = npoints
    !
    givenDischarge = 1d-3
    !
    ! test actual computations in iterateToGivenDischarge; ...
    !
    ! water level below first lowest profile point
    load%h        =  -4.5_wp
    load%phi      = 270.0_wp
    load%Hm0      =   0.8_wp
    load%Tm_10    =   3.3_wp
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, -4.5_wp, 1e-4_wp,    'dikeHeight from omkeer variant, low water')
    call assert_comparable(overtopping%z2, 0.0_wp, 1e-4_wp, 'z2 from omkeer variant        , low water')

    ! water level on profile
    load%h        =  -1.0_wp
    load%phi      = 270.0_wp
    load%Hm0      =   0.8_wp
    load%Tm_10    =   3.3_wp
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 0.2534_wp, 1e-4_wp,    'dikeHeight from omkeer variant, intermediate water')
    call assert_comparable(overtopping%z2, 1.52108_wp, 1e-4_wp, 'z2 from omkeer variant        , intermediate water')

    ! water level above highest profile point
    load%h        =   4.5_wp
    load%phi      = 270.0_wp
    load%Hm0      =   0.8_wp
    load%Tm_10    =   3.3_wp
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 5.7534_wp, 1e-4_wp,    'dikeHeight from omkeer variant, high water')
    call assert_comparable(overtopping%z2, 1.52108_wp, 1e-4_wp, 'z2 from omkeer variant        , high water')

    ! wave height zero
    load%h        =  -1.0_wp
    load%phi      = 270.0_wp
    load%Hm0      =   0.0_wp
    load%Tm_10    =   3.3_wp
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, -1.0_wp, 1e-4_wp,    'dikeHeight from omkeer variant, wave height zero')
    call assert_comparable(overtopping%z2, 0.0_wp, 1e-4_wp, 'z2 from omkeer variant        , wave height zero')

    ! offshore directed waves
    load%h        =  -1.0_wp
    load%phi      =  90.0_wp
    load%Hm0      =   0.8_wp
    load%Tm_10    =   3.3_wp
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, -1.0_wp, 1e-4_wp,    'dikeHeight from omkeer variant, offshore directed waves')
    call assert_comparable(overtopping%z2, 0.0_wp, 1e-4_wp, 'z2 from omkeer variant        , offshore directed waves')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantLoadTests

!> test for issue Overs-52 (two berm segments)
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantIssue52
    integer, parameter             :: npoints = 5
    type (tpOvertopping)           :: overtopping
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    type(tMessage)                 :: error

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)
    load%h        =  3.0_wp
    load%phi      =  0.0_wp
    load%Hm0      =  2.0_wp
    load%Tm_10    =  5.65997611606080_wp

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%roughness = 1.0_wp
    geometryF%xcoords   = [0.0_wp, 24.15_wp, 28.15_wp, 32.15_wp, 40.55_wp]
    geometryF%ycoords   = [-2.0_wp, 3.75_wp,  4.0_wp, 4.0_wp, 6.0_wp]

    geometryF%normal = 0.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge
    !
    givenDischarge = 1D-3
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
    call assert_equal(error%errorCode, 0, error%Message)
    call assert_comparable(dikeHeight, 5.9821_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 2.9140_wp, 0.001_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantIssue52

end module omkeerVariantTests
