!> @file
!! Contains the module omkeerVariantTests of the DikesOvertopping dll
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!> 
!! Module, holding tests of the 'omkeerVariant' functions within the dll.
!!
!! @ingroup DikeOvertoppingTests
module omkeerVariantTests
use precision, only : wp
use typeDefinitionsRTOovertopping
use overtoppingInterface
use ModuleLogging
use ftnunit
use waveParametersUtilities
use omkeerVariantModule
implicit none

private

public :: allOmkeerVariantTests

contains

!> Performs all tests
subroutine allOmkeerVariantTests
    call testWithLevel( omkeerVariantTest1, "inverse of overtoppingDllTest test", 1)
    call testWithLevel( omkeerVariantTest2, "inverse of overtoppingDllTest test, high discharge", 1)
    call testWithLevel( omkeerVariantTest2b, "inverse of overtoppingDllTest test, expected dikeheight in profile", 1)
    call testWithLevel( omkeerVariantTest3, "inverse of overtoppingDllTest test, with berm", 1)
    call testWithLevel( omkeerVariantTest4, "inverse of overtoppingDllTest test, with berm and dikeheight just above berm", 1)
    call testWithLevel( omkeerVariantTest5, "inverse of overtoppingDllTest test, with berm and expected dikeheight just above berm", 1)
    call testWithLevel( omkeerVariantTest6, "inverse of overtoppingDllTest test, with 1:15 berm and expected dikeheight halfway berm", 1)
end subroutine allOmkeerVariantTests

! inverse of overtoppingDllTest test:
!! @ingroup FailureMechanismsTests
subroutine omkeerVariantTest1
    use user32
    use kernel32

    real(kind=wp), parameter :: margin     =  0.00001_wp

    integer                        :: p
    external                       :: omkeerVariant
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: ierr              !< error code
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
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    criticalOvertoppingRate                 = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness, ierr, errorMessage )
    call assert_equal( ierr, 0, errorMessage )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    givenDischarge = 0.8d-8
    call omkeerVariant( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( dikeHeight, 9.1d0, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable ( overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable ( overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTest1

! as omkeerVariantTest1, but not through dll and max. discharge 
!! @ingroup FailureMechanismsTests
subroutine omkeerVariantTest2
    real(kind=wp), parameter :: margin     =  0.00001_wp
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: ierr              !< error code
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

    !
    ! initializations
    !
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    criticalOvertoppingRate                 = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness, ierr, errorMessage )
    call assert_equal( ierr, 0, errorMessage )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    givenDischarge = 0.2d0
    call iterateToGivenDischarge( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( dikeHeight, 5.522_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable ( overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable ( overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTest2

! as omkeerVariantTest2, but with a expected discharge near y(2)
!! @ingroup FailureMechanismsTests
subroutine omkeerVariantTest2b
    real(kind=wp), parameter :: margin     =  0.00001_wp
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: ierr              !< error code
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

    !
    ! initializations
    !
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    criticalOvertoppingRate                 = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness, ierr, errorMessage )
    call assert_equal( ierr, 0, errorMessage )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    givenDischarge = 1.568D-4  ! exact : 1.565674960996936D-004
    call iterateToGivenDischarge( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( dikeHeight, 7.025_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable ( overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable ( overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTest2b

! as omkeerVariantTest2, but with berm
!! @ingroup FailureMechanismsTests
subroutine omkeerVariantTest3
    real(kind=wp), parameter :: margin     =  0.00001_wp
    integer                        :: i, ii
    logical                        :: succes
    integer, parameter             :: npoints = 4
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: ierr              !< error code
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

    !
    ! initializations
    !
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    criticalOvertoppingRate                 = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if ( i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness, ierr, errorMessage )
    call assert_equal( ierr, 0, errorMessage )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    givenDischarge = 0.2d0
    call iterateToGivenDischarge( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( dikeHeight, 5.522_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable ( overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable ( overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTest3

! as omkeerVariantTest3, but with dikeheight just above berm
!! @ingroup FailureMechanismsTests
subroutine omkeerVariantTest4
    real(kind=wp), parameter :: margin     =  0.00001_wp
    integer                        :: i, ii
    logical                        :: succes
    integer, parameter             :: npoints = 4
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: ierr              !< error code
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

    !
    ! initializations
    !
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    criticalOvertoppingRate                 = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if ( i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%ycoords(npoints) = geometryF%ycoords(npoints-1) + .5_wp
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness, ierr, errorMessage )
    call assert_equal( ierr, 0, errorMessage )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    givenDischarge = 0.2d0
    call iterateToGivenDischarge( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( dikeHeight, 5.522_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable ( overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable ( overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTest4

! as omkeerVariantTest4, but with expected dikeheight just above berm
!! @ingroup FailureMechanismsTests
subroutine omkeerVariantTest5
    real(kind=wp), parameter :: margin     =  0.00001_wp
    integer                        :: i, ii
    logical                        :: succes
    integer, parameter             :: npoints = 4
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: ierr              !< error code
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to

    !
    ! initializations
    !
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    criticalOvertoppingRate                 = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if ( i /= 3) ii = ii + 1
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * ii
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%ycoords(npoints) = geometryF%ycoords(npoints-1) + .5_wp
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness, ierr, errorMessage )
    call assert_equal( ierr, 0, errorMessage )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    givenDischarge = 1.763396051957844D-004
    call iterateToGivenDischarge( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( dikeHeight, 7.000_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable ( overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable ( overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTest5

! as omkeerVariantTest5, but with 1:15 berm, and expected dikeheight halfway berm
!! @ingroup FailureMechanismsTests
subroutine omkeerVariantTest6
    real(kind=wp), parameter :: margin     =  0.00001_wp
    integer                        :: i, ii
    logical                        :: succes
    integer, parameter             :: npoints = 4
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: ierr              !< error code
    real(kind=wp)                  :: givenDischarge    !< discharge to iterate to
    real(kind=wp)                  :: bermLength
    real(kind=wp)                  :: dy

    !
    ! initializations
    !
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    criticalOvertoppingRate                 = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    ii = 0
    do i = 1, npoints
        if ( i /= 3) ii = ii + 1
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
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness, ierr, errorMessage )
    call assert_equal( ierr, 0, errorMessage )
    !
    ! test actual computations in iterateToGivenDischarge for waterlevel < dikeheigth
    !
    givenDischarge = 8.005817025697209D-5
    call iterateToGivenDischarge( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( dikeHeight, 7.166_wp, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable ( overtopping%z2, 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable ( overtopping%qo, givenDischarge, 1d-3, 'discharge last iteration from omkeer variant')

    ! clean up
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine omkeerVariantTest6

end module omkeerVariantTests
