!> @file
!! Contains the module omkeerVariantTests of the DikesOvertopping dll
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!> 
!! Module, holding tests of the 'omkeerVariant' functions within the DikesOvertopping dll.
!!
!! @ingroup DikeOvertoppingTests
module omkeerVariantTests
use precision, only : wp
use typeDefinitionsRTOovertopping
use overtoppingInterface
use ModuleLogging
use ftnunit
use testHelper, only : init_modelfactors_and_load
use omkeerVariantModule
use user32
use kernel32

implicit none

private

public :: allOmkeerVariantTests

contains

!> Performs all tests
subroutine allOmkeerVariantTests
    call testWithLevel(omkeerVariantTestBasic,                                "omkeerVariantTest: inverse of overtoppingDllTest test", 1)
    call testWithLevel(omkeerVariantTestWithHighDischarge,                    "omkeerVariantTest: high discharge", 1)
    call testWithLevel(omkeerVariantTestWithDikeHeigthInProfile,              "omkeerVariantTest: expected dikeheight in profile", 1)
    call testWithLevel(omkeerVariantTestWithBerm,                             "omkeerVariantTest: with berm", 1)
    call testWithLevel(omkeerVariantTestWithBermAndDikeHeightJustAboveBerm1,  "omkeerVariantTest: with berm and dikeheight just above berm", 1)
    call testWithLevel(omkeerVariantTestWithBermAndDikeHeightJustAboveBerm2,  "omkeerVariantTest: with berm and expected dikeheight just above berm", 1)
    call testWithLevel(omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm, "omkeerVariantTest: with 1:15 berm and expected dikeheight halfway berm", 1)
    call testWithLevel(omkeerVariantTestWithVerySmallDischarge,               "omkeerVariantTest: with a very small discharge", 1)
    call testWithLevel(omkeerVariantTestWithWaterlevelBelowToe,               "omkeerVariantTest: with water level below toe", 1)
    call testWithLevel(omkeerVariantTestWithExpectedDikeheightHalfwaySlope,   "omkeerVariantTest: with expected dikeheight halfway last slope segment", 1)
end subroutine allOmkeerVariantTests

!> inverse of overtoppingDllTest test:
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestBasic
    integer                        :: p
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

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    call assert_true(p /= 0, 'load dllDikesOvertopping.dll')
    q = getprocaddress (p, "omkeerVariantF"C)
    call assert_true(q /= 0, 'get function pointer to omkeerVariant')
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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
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
    logical                        :: succes
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
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
    logical                        :: succes
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
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
    logical                        :: succes
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
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
    logical                        :: succes
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy

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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
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
    logical                        :: succes
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy

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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
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
    logical                        :: succes
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy

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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_comparable(dikeHeight, load%h, 1d-15, 'dikeHeight from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithWaterlevelBelowToe

!> as omkeerVariantTestWith1_15BermAndDikeHeightHalfwayBerm, but expected dikeheight halfway last slope segment
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestWithExpectedDikeheightHalfwaySlope
    integer                        :: i
    integer                        :: ii
    logical                        :: succes
    integer, parameter             :: npoints = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy

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
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_comparable(dikeHeight, 7.856_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTestWithExpectedDikeheightHalfwaySlope

end module omkeerVariantTests
