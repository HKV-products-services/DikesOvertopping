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
!! Contains the module dllTests of the DikesOvertopping dll
!
! $Id$
!
!> 
!! Module, holding tests of the functions of the dll.
!!
!! @ingroup DikeOvertoppingTests
module dllTests
use precision, only : wp, pntlen, set_nan
use overtoppingInterface, only : OvertoppingGeometryTypeF
use typeDefinitionsOvertopping
use ModuleLogging
use waveParametersUtilities, only : computeWavePeriod
use zFunctionsOvertopping, only : profileInStructure
use testHelper, only : init_modelfactors_and_load
use geometryModuleOvertopping, only : cleanupCoordinatePair
use ftnunit
use errorMessages
use user32
use kernel32

implicit none

private

public :: allOvertoppingDllTests

contains

subroutine allOvertoppingDllTests
    call testWithLevel(overtoppingDllTest,                 'General; Test functions versionNumber, calculateQoF, calcZValue in the dll', 1)
    call testWithLevel(overtoppingDikeInProfileTest,       'General; Test a dikeheight at one of the profile points', 1)
    call testWithLevel(influenceRoughnessTest,             'General; Test influence roughness', 1)
    call testWithLevel(overtoppingValidationTest,          'General; Test validation of incorrect profile and negative model factor', 1)
    call testWithLevel(overtoppingValidationRoughnessTest, 'General; Test validation of invalid roughness', 1)
    call testWithLevel(overtoppingMultipleValidationTest,  'General; Test validation of incorrect profile and negative model factor in one call', 1)
    call testWithLevel(overtoppingValidationTestZPoints,   'General; Test message of incorrect profile (z-value)', 1)
    call testWithLevel(overtoppingValidationTestInvalidSlope,'General: Test message of invalid slope', 1)
    call testWithLevel(overtoppingZ2Test,                  'General; Test h+z2 > dikeheight', 1)
    call testWithLevel(LoadNaNTest,                        'General; Test error handling in case of NaN in load', 1)
    call testWithLevel(TestProfileAdjustment,              'General; Test whether the profile is adapted correctly', 1)
    call testWithLevel(TestRoughnessIssue44A,              'General; ISSUE; Test A for calculateGammaF related to issue 44', 1)
    call testWithLevel(TestRoughnessIssue44B,              'General; ISSUE; Test B for calculateGammaF related to issue 44', 1)
    call testWithLevel(TestIssue45,                        'General; ISSUE; Test for issue 45', 1)
    call testWithLevel(overtoppingDllTest2,                'Uniform Slope; Test the dll for a uniform slope (18 cases)', 1)
    call testWithLevel(TestOmkeervar,                      'General; ISSUE; Omkeervar', 1)
    call testWithLevel(TestIssue52,                        'General; ISSUE; two berms', 1)
end subroutine allOvertoppingDllTests

!> Test the functions in DikesOvertopping.dll.
!!     these functions are:
!!     - calcZValue
!!     - calculateQoF
!!     - versionNumber
!!
!!     - test overflow (waterlevel > dike height)
!!     - test with and without waves
!!
!! @ingroup DikeOvertoppingTests
subroutine overtoppingDllTest
    real(kind=wp)      :: z                 !< z value

    real(kind=wp), parameter :: zExpected1a =   11.725
    real(kind=wp), parameter :: zExpected1b =  701.48866_wp
    real(kind=wp), parameter :: zExpected2 = -6.43985_wp
    real(kind=wp), parameter :: margin     =  0.0000100_wp

    integer(kind=pntlen)           :: p
    character(len=12)              :: version
    external                       :: calcZValue
    external                       :: calculateQoF
    external                       :: versionNumber
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging

    pointer            (qz, calcZValue)
    pointer            (qv, versionNumber)
    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qv = getprocaddress (p, "versionNumber"C)
    qc = getprocaddress (p, "calculateQoF"C)
    qz = getprocaddress (p, "calcZValue"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactors, load)
    criticalOvertoppingRate        = 1.0d-3

    call versionNumber(version)
    call assert_false('0.0.0.0' == version, "version not set")

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheight
    !
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_comparable(overtopping%Qo, 0.808902537811215d-8, margin, 'Qo from dllOvertopping.dll')
    call assert_comparable(overtopping%z2, 1.51985829057_wp, margin, 'z2 from dllOvertopping.dll')
    call calcZValue(criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage)
    call assert_true(succes, errorMessage)
    call assert_comparable(z, zExpected1a, margin, "Z value from dllOvertopping.dll; overflow")
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel > dikeheight (overflow)
    !
    load%h        =  9.50_wp
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_false(succes, errorMessage)
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheight, without waves
    !
    load%h        =  5.50_wp
    load%Hm0      =  0.00_wp
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call calcZValue(criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage)
    call assert_comparable(z, zExpected1b, margin, "Z value from dllOvertopping.dll; no waves test")

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingDllTest

!! test Dike at one of the profile points
!! @ingroup DikeOvertoppingTests
subroutine overtoppingDikeInProfileTest
    integer(kind=pntlen)           :: p
    external                       :: calculateQoF
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging

    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)
    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors, load)
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    dikeHeight  = 7.0_wp
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingDikeInProfileTest

!> Test the functions in dllOvertopping.dll.
!!     these functions are:
!!     - calculateQoF
!!
!!     - test overflow(waterlevel > dike height)
!!     - test with and without waves
!!
!! @ingroup DikeOvertoppingTests
subroutine overtoppingZ2Test
    real(kind=wp), parameter :: zExpected1a =   29.88985_wp
    real(kind=wp), parameter :: zExpected1b =  701.48866_wp
    real(kind=wp), parameter :: zExpected2 = -6.43985_wp
    real(kind=wp), parameter :: margin     =  0.0000100_wp

    integer(kind=pntlen)           :: p
    external                       :: calculateQoF
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging

    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactors, load, 8.99_wp)
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheight
    !
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_comparable(overtopping%Qo, 0.48213d0, margin, 'Qo from dllOvertopping.dll')
    call assert_comparable(overtopping%z2,  2.874674352d0, margin, 'z2 from dllOvertopping.dll')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingZ2Test

!> Test influence roughness in dllOvertopping.dll.
!!
!!
!! @ingroup DikeOvertoppingTests
subroutine influenceRoughnessTest
    integer(kind=pntlen)           :: p
    external                       :: calculateQoF
    logical                        :: succes
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging

    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)
    !
    ! initializations
    !
    dikeHeight = 3.7_wp
    call init_modelfactors_and_load(modelFactors, load, -0.361314622129615_wp)
    load%phi   = 45.0_wp
    load%hm0   = 1d-6
    load%tm_10 = 1.912229230397281D-012

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords(1) =  0.0_wp
    geometryF%xcoords(2) = 24.0_wp
    geometryF%ycoords(1) = -3.0_wp
    geometryF%ycoords(2) =  3.0_wp
    geometryF%roughness  =  1.0_wp
    geometryF%normal     =  0.0_wp
    geometryF%npoints    =  npoints
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheight
    !
    logging%verbosity = -1
    logging%filename = ' '
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_inbetween(overtopping%z2, 1d-16, 2d-15, 'difference in z2')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine influenceRoughnessTest

!> Test NaN in load in dllOvertopping.dll.
!!
!! @ingroup DikeOvertoppingTests
subroutine LoadNaNTest
    integer(kind=pntlen)           :: p
    external                       :: calculateQoF
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging

    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)
    !
    ! initializations
    !
    dikeHeight = 3.7_wp
    call init_modelfactors_and_load(modelFactors, load)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords(1) =  0.0_wp
    geometryF%xcoords(2) = 24.0_wp
    geometryF%ycoords(1) = -3.0_wp
    geometryF%ycoords(2) =  3.0_wp
    geometryF%roughness  =  1.0_wp
    geometryF%normal     =  0.0_wp
    geometryF%npoints    =  npoints
    !
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheight
    !
    logging%verbosity = -1
    logging%filename = ' '
    do i = 1, 4
        load%h     = -0.361314622129615_wp
        load%phi   = 45.0_wp
        load%hm0   = 1d-6
        load%tm_10 = 1.912229230397281D-012
        select case(i)
        case(1)
            call set_nan(load%h)
        case(2)
            call set_nan(load%phi)
        case(3)
            call set_nan(load%hm0)
        case(4)
            call set_nan(load%tm_10)
        end select
        call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
        call assert_false(succes, errorMessage)
    enddo

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine LoadNaNTest

!> Test the validation functions in dllOvertopping.dll.
!!
!! @ingroup DikeOvertoppingTests
subroutine overtoppingValidationTest
    integer(kind=pntlen)           :: p
    external                       :: ValidateInputF, SetLanguage
    integer, parameter             :: npoints = 5
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(TErrorMessages)           :: errorStruct

    pointer            (qc, ValidateInputF)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputF"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactors)
    criticalOvertoppingRate          = 1.0d-3
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 10, 20, 30, 40 ]
    geometryF%ycoords = [-5, 0, -1, 4, 0]
    geometryF%roughness = [ 0.5, 0.5, 0.5, 0.5 ]
    
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! do validation of input (geometry not correct) :
    !
    call initErrorMessages(errorStruct)
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    
    call assert_equal(errorStruct%messages(1)%message, 'Verticale coordinaten mogen niet afnemen.   0.00 en   -1.00 doen dat wel.', 'error handling')
    call assert_equal(errorStruct%messages(2)%message, 'Verticale coordinaten mogen niet afnemen.   4.00 en    0.00 doen dat wel.', 'error handling')

    !
    ! do validation of input (modelfactor m_z2 < 0) :
    !
    geometryF%ycoords = [-5, 0, 5, 6, 7]
    modelFactors%m_z2     = -1.00_wp
    call SetLanguage('UK')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    
    call assert_equal(errorStruct%messages(4)%message, 'Model factor 2% wave runup smaller than  0.000', 'error handling')
    
    !
    ! do validation of input (modelfactor foreshore < 0.3) :
    !
    modelFactors%m_z2                      = 1.00_wp
    modelFactors%reductionFactorForeshore  = 0.25_wp
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call assert_equal(errorStruct%messages(6)%message, 'Model factor reduction factor foreshore not between  0.300 and  1.000', 'error handling')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingValidationTest

subroutine overtoppingValidationRoughnessTest
    integer(kind=pntlen)           :: p
    external                       :: ValidateInputF, SetLanguage
    integer, parameter             :: npoints = 5
    type(TErrorMessages)           :: errorStruct
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qc, ValidateInputF)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputF"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactors)
    criticalOvertoppingRate          = 1.0d-3
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 10, 20, 30, 40 ]
    geometryF%ycoords = [-5, 0, 5, 6, 7]
    geometryF%roughness = [ 0.0, 0.5, 0.5, 0.5 ]
    
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! do validation of input (geometry not correct) :
    !
    call SetLanguage('NL')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    
    call assert_equal(errorStruct%messages(1)%message, 'Ruwheidsfactoren moeten liggen tussen 0.5 ... 1.0; gevonden:  0.00', 'error handling')

    geometryF%roughness = [ 0.49, 0.5, 0.5, 0.5 ]
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    
    call assert_equal(errorStruct%messages(2)%message, 'Ruwheidsfactoren moeten liggen tussen 0.5 ... 1.0; gevonden:  0.49', 'error handling')
    !
    ! do validation of input (modelfactor m_z2 < 0) :
    !
end subroutine overtoppingValidationRoughnessTest

!! @ingroup DikeOvertoppingTests
subroutine overtoppingMultipleValidationTest
    integer(kind=pntlen)           :: p
    external                       :: ValidateInputF, SetLanguage
    integer, parameter             :: npoints = 5
    type(TErrorMessages)           :: errorStruct
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qc, ValidateInputF)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputF"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactors)
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 10, 20, 30, 40 ]
    geometryF%ycoords = [-5, 0, 5, 4, 0]
    geometryF%roughness = [ 0.5, 0.5, 0.5, 0.5 ]
    
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    modelFactors%m_z2     = -1.00_wp
    modelFactors%reductionFactorForeshore  = 0.25_wp
    !
    ! do validation of input :
    !
    call initErrorMessages(errorStruct)
    call SetLanguage('UK')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)

    call assert_equal(4, errorStruct%nErrors, 'expected 4 validation errors')
    call assert_equal(trim(errorStruct%messages(1)%message), 'Coordinates in vertical direction must be non-decreasing.   5.00 and    4.00 are not.', 'error handling')
    
    call assert_equal(trim(errorStruct%messages(2)%message), 'Coordinates in vertical direction must be non-decreasing.   4.00 and    0.00 are not.', 'error handling')

    call assert_equal(trim(errorStruct%messages(3)%message), 'Model factor 2% wave runup smaller than  0.000', 'error handling')
    
    call assert_equal(trim(errorStruct%messages(4)%message), 'Model factor reduction factor foreshore not between  0.300 and  1.000', 'error handling')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingMultipleValidationTest

!! @ingroup DikeOvertoppingTests
subroutine overtoppingValidationTestZPoints
    integer(kind=pntlen)           :: p
    external                       :: ValidateInputF, SetLanguage
    integer, parameter             :: npoints = 3
    type(TErrorMessages)           :: errorStruct
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qc, ValidateInputF)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputF"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 0 ! 9.1_wp
    call init_modelfactors_and_load(modelFactors)
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 40, 80 ]
    geometryF%ycoords = [-10, 0, -10]
    geometryF%roughness = [ 1.0, 1.0 ]
    
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! do validation of input :
    !
    call initErrorMessages(errorStruct)
    call SetLanguage('NL')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call SetLanguage('UK')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)

    call assert_equal(2, errorStruct%nErrors, 'expected 2 validation errors')
    call assert_equal(errorStruct%messages(1)%message, 'Verticale coordinaten mogen niet afnemen.   0.00 en  -10.00 doen dat wel.', 'error handling')
    call assert_equal(errorStruct%messages(2)%message, 'Coordinates in vertical direction must be non-decreasing.   0.00 and  -10.00 are not.', 'error handling')
    
    geometryF%xcoords = [ 0d0, 40d0, 40.019d0 ]
    geometryF%ycoords = [-10d0, 0d0, 10d0]
    call SetLanguage('NL')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call SetLanguage('UK')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call assert_equal(4, errorStruct%nErrors, 'expected 4 (total) validation errors')
    call assert_equal(errorStruct%messages(3)%message, 'X-coordinaten moeten ten minste 0.02 van elkaar verschillen.  40.000 en   40.019 ligt te dicht bij elkaar.', 'error handling')
    call assert_equal(errorStruct%messages(4)%message, 'X-coordinates must differ at least 0.02.  40.000 and   40.019 are too close to each other.', 'error handling')

    geometryF%xcoords = [ 0d0, 40d0, 40.0201d0 ]
    geometryF%ycoords = [ 0d0, 9.9d0, 9.91d0]
    call SetLanguage('NL')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call assert_equal(4, errorStruct%nErrors, 'expected 4 (total) validation errors')

    geometryF%xcoords = [ 0d0, 40d0, 39.0d0 ]
    geometryF%ycoords = [-10d0, 0d0, 10d0]
    call SetLanguage('NL')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call SetLanguage('UK')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call assert_equal(6, errorStruct%nErrors, 'expected 6 (total) validation errors')
    call assert_equal(errorStruct%messages(5)%message, 'X-coordinaten moeten ten minste met 0.02 toenemen.  40.000 en   39.000 voldoen hier niet aan.', 'error handling')
    call assert_equal(errorStruct%messages(6)%message, 'X-coordinates must increase at least with 0.02.  40.000 and   39.000 do not meet this condition.', 'error handling')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingValidationTestZPoints

!! @ingroup DikeOvertoppingTests
subroutine overtoppingValidationTestInvalidSlope
    integer(kind=pntlen)           :: p
    external                       :: ValidateInputF, SetLanguage
    integer, parameter             :: npoints = 3
    type(TErrorMessages)           :: errorStruct
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qc, ValidateInputF)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputF"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  =  9.1_wp
    call init_modelfactors_and_load(modelFactors)
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 40, 80 ]
    geometryF%ycoords = [-10, 0, 41]
    geometryF%roughness = [ 1.0, 1.0 ]

    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! do validation of input :
    !
    call initErrorMessages(errorStruct)
    call SetLanguage('NL')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call SetLanguage('UK')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)

    call assert_equal(2, errorStruct%nErrors, 'expected 2 validation errors')
    call assert_equal(errorStruct%messages(1)%message, 'Dijk segment is van ander type dan berm segment of helling segment', 'error handling')
    call assert_equal(errorStruct%messages(2)%message, 'Dike segment mismatches berm segment or slope segment', 'error handling')
end subroutine overtoppingValidationTestInvalidSlope

!> test for the adjustment of the cross section
!!
!! @ingroup DikeOvertoppingTests
subroutine TestProfileAdjustment
!
!   Local parameters
!
    type(tpCoordinatePair)     :: coordinates          !< x/y coordinates
    type(tpCoordinatePair)     :: coordsAdjusted       !< x/y coordinates of the adjusted profile
    real(kind=wp)              :: dikeHeight           ! vector with x-coordinates of the adjusted profile
    integer                    :: i                    ! do-loop counter
    type(tMessage)             :: error                ! error struct

    Coordinates%N = 4
    allocate (Coordinates%x(Coordinates%N))
    allocate (Coordinates%y(Coordinates%N))

    Coordinates%x = (/ 28.5d0, 30.55d0, 34.17d0, 37.0d0 /)

    Coordinates%y = (/ 0.02d0, 0.36d0, 1.12d0, 1.74d0 /)

    dikeHeight = 0.74d0

    call profileInStructure(Coordinates,dikeHeight, coordsAdjusted, error)
    
    do i = 2, CoordsAdjusted%N
        call assert_true (CoordsAdjusted%x(i) > CoordsAdjusted%x(i-1), "X coordinates increasing")
    enddo

    call cleanupCoordinatePair(Coordinates)
    call cleanupCoordinatePair(CoordsAdjusted)

end subroutine TestProfileAdjustment

!> basic test related to issue 44 (this situation went wrong)
!! @ingroup DikeOvertoppingTests
subroutine TestRoughnessIssue44A
!
!   Local parameters
!
    integer                        :: npoints        ! number of coordinates
    logical                        :: succes         ! flag for succes
    character(len=255)             :: errorMessage   ! error message
    real(kind=wp)                  :: dikeHeight     ! vector with x-coordinates of the adjusted profile
    type (tpLoad)                  :: load           !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging        !< logging struct
    type (tpOvertopping)           :: overtopping    !< structure with overtopping results
    integer(kind=pntlen)           :: p

    pointer(qq  , calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qq   = getprocaddress (p, "calculateQoF"C)

    npoints = 4
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%npoints = npoints
    geometryF%roughness = 1.0_wp
    geometryF%xcoords = [ -1.73_wp, 33.82_wp, 38.16_wp, 47.34_wp ]
    geometryF%ycoords = [ -2.89_wp, 6.03_wp, 6.31_wp, 8.64_wp ]
    
    load%h     =     1.61308120110_wp
    load%Hm0   =     0.204602978227d-06
    load%Tm_10 =     0.109818132516d-10
    load%phi   =    60.0000225877_wp
    dikeHeight =     7.10654257734_wp

    call init_modelfactors_and_load(modelFactors)
    modelFactors%fshallow = 0.6778_wp
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_comparable(overtopping%Qo, 0.0_wp, 1d-15, "expect Q0 = 0")

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine TestRoughnessIssue44A

!> 2nd test related to issue 44
!! (more complex profile; manual check in debugger that right roughness is used)
!! @ingroup DikeOvertoppingTests
subroutine TestRoughnessIssue44B
!
!   Local parameters
!
    integer                        :: npoints        ! number of coordinates
    logical                        :: succes         ! flag for succes
    character(len=255)             :: errorMessage   ! error message
    real(kind=wp)                  :: dikeHeight     ! vector with x-coordinates of the adjusted profile
    type (tpLoad)                  :: load           !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging        !< logging struct
    type (tpOvertopping)           :: overtopping    !< structure with overtopping results
    integer(kind=pntlen)           :: p

    pointer(qq  , calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qq   = getprocaddress (p, "calculateQoF"C)

    npoints = 5
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%npoints = npoints
    geometryF%roughness = [1.0_wp, 0.9_wp, 0.8_wp, 0.7_wp]
    geometryF%xcoords = [ -5.0_wp, -1.73_wp, 33.82_wp, 38.16_wp, 47.34_wp ]
    geometryF%ycoords = [ -4.0_wp, -2.89_wp, 6.03_wp, 6.31_wp, 8.64_wp ]

    load%h     =     1.61308120110_wp
    load%Hm0   =     0.204602978227d-06
    load%Tm_10 =     0.109818132516d-10
    load%phi   =    60.0000225877_wp
    dikeHeight =     7.10654257734_wp

    call init_modelfactors_and_load(modelFactors)
    modelFactors%fshallow = 0.6778_wp
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
    call assert_comparable(overtopping%Qo, 0.0_wp, 1d-15, "expect Q0 = 0")

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine TestRoughnessIssue44B

!> check cause of unclear error message
subroutine TestIssue45
    integer                        :: npoints             ! number of coordinates
    real(kind=wp), parameter       :: dikeHeight = 6.5_wp
    type(OvertoppingGeometryTypeF) :: geometryF
    type(tpOvertoppingInput)       :: modelFactors
    type(TErrorMessages)           :: errorStruct
    character(len=2)               :: lang
    integer(kind=pntlen)           :: p

    pointer(qVal, ValidateInputF)
    pointer(qGetLang, GetLanguage)
    pointer(qSetLang, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qVal = getprocaddress (p, "ValidateInputF"C)
    qGetLang = getprocaddress (p, "GetLanguage"C)
    qSetLang = getprocaddress (p, "SetLanguage"C)

    call GetLanguage(lang)
    call SetLanguage('NL')
    npoints = 6
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%npoints = npoints
    geometryF%roughness = [0.9_wp, 0.9_wp, 1.0_wp, 1.0_wp, 1.0_wp]
    geometryF%xcoords = [ -50.0_wp, -5.75_wp, 3.25_wp, 13.25_wp, 17.25_wp, 30.0_wp ]
    geometryF%ycoords = [ -4.0_wp, -3.0_wp, -0.13_wp, 3.06_wp, 4.33_wp, 6.5_wp ]
    geometryF%normal = 63.0_wp

    !Input ReductionFactorForeshore = 0.5
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%fshallow                   = 0.677780705266081_wp
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0_wp

    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call assert_equal(errorStruct%nErrors, 1, "expect 1 error message about berms")
    call assert_equal(errorStruct%messages(1)%message, 'Eerste segment is een berm. Dat is niet toegestaan.', 'error in validation message')

    call SetLanguage(lang)

end subroutine TestIssue45

!> Test the functions calculateQoF and calcZValue in DikesOvertopping.dll
!!     simple case, can be computed manually
!!
!! @ingroup DikeOvertoppingTests
subroutine overtoppingDllTest2
    real(kind=wp)                  :: z                 !< z value

    real(kind=wp), parameter       :: margin = 0.0000100_wp

    integer(kind=pntlen)           :: p
    external                       :: calcZValue
    external                       :: calculateQoF
    logical                        :: succes
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging
    integer                        :: testNr
    real(kind=wp)                  :: z2Expected
    real(kind=wp)                  :: qoExpected
    real(kind=wp)                  :: zExpected
    character(len=128)             :: z2String
    character(len=128)             :: qoString
    character(len=128)             :: zString

    pointer            (qz, calcZValue)
    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)
    qz = getprocaddress (p, "calcZValue"C)
    
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))

    do testNr=1,18
        ! =====  initializations =====
        !
        ! model factors
        !
        modelFactors%factorDeterminationQ_b_f_n = 2.6_wp
        modelFactors%factorDeterminationQ_b_f_b = 4.75_wp
        modelFactors%m_z2                       = 1.00_wp
        modelFactors%fshallow                   = 0.92_wp
        modelFactors%ComputedOvertopping        = 1.0_wp
        modelFactors%CriticalOvertopping        = 1.0_wp
        modelFactors%relaxationFactor           = 1.0d0
        !
        ! structure parameters
        !
        if (testNr<=9) then
            geometryF%xcoords   = [ 0.0_wp, 40.0_wp]
        else
            geometryF%xcoords   = [ 0.0_wp, 20.0_wp]
        endif
        geometryF%ycoords   = [-5.0_wp,  5.0_wp]
        geometryF%roughness = 1.0_wp
        geometryF%normal    = 0.0_wp ! degrees
        geometryF%npoints   = npoints
        dikeHeight          = 5.0_wp
        criticalOvertoppingRate = 1.0d-3
        !
        ! load parameters
        !
        load%h     =     2.0_wp
        load%Hm0   =     1.0_wp
        load%Tm_10 =     4.0_wp
        load%phi   =     0.0_wp
        !
        select case (testNr)
            case (1)
                z2Expected = 2.06136_wp
                qoExpected = 5.83238d-06
                zExpected  = 5.14433_wp
            case (2)
                load%h     = 2.5_wp
                z2Expected = 2.06136_wp
                qoExpected = 3.90356d-05
                zExpected  = 3.24328_wp
            case (3)
                load%Hm0   = 0.8_wp
                z2Expected = 1.84374_wp
                qoExpected = 1.21404d-06
                zExpected  = 6.71380_wp
            case (4)
                load%Tm_10 = 5.0_wp
                z2Expected = 2.57670_wp
                qoExpected = 7.13664d-05
                zExpected  = 2.63993_wp
            case (5)
                load%phi   = 45.0_wp
                z2Expected = 1.85729_wp
                qoExpected = 7.97870d-07
                zExpected  = 7.13357_wp
            case (6)
                dikeHeight = 4.0_wp
                z2Expected = 2.06136_wp
                qoExpected = 2.61262d-04
                zExpected  = 1.34223_wp
            case (7)
                geometryF%roughness = 0.7_wp
                z2Expected = 1.44295_wp
                qoExpected = 4.39376d-08
                zExpected  = 10.03274_wp
            case (8)
                modelFactors%m_z2                = 1.10_wp
                modelFactors%CriticalOvertopping = 0.9_wp
                z2Expected = 2.26750_wp
                qoExpected = 5.83238d-06
                zExpected  = 5.03897_wp
            case (9)
                modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
                modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
                z2Expected = 2.06136_wp
                qoExpected = 1.71847d-05
                zExpected  = 4.06373_wp
            case (10)
                z2Expected = 3.05105_wp
                qoExpected = 2.56622d-04
                zExpected  = 1.36015_wp
            case (11)
                load%h     = 2.5_wp
                z2Expected = 3.05105_wp
                qoExpected = 9.41621d-04
                zExpected  = 6.01523d-02
            case (12)
                load%Hm0   = 0.8_wp
                z2Expected = 2.48203_wp
                qoExpected = 2.61249d-05
                zExpected  = 3.64487_wp
            case (13)
                load%Tm_10 = 5.0_wp
                z2Expected = 3.15124_wp
                qoExpected = 2.56622d-04
                zExpected  = 1.36015_wp
            case (14)
                load%phi   = 45.0_wp
                z2Expected = 2.74900_wp
                qoExpected = 6.58446d-05
                zExpected  = 2.72046_wp
            case (15)
                dikeHeight = 4.0_wp
                z2Expected = 3.05105_wp
                qoExpected = 3.45509d-03
                zExpected  = -1.23985_wp
            case (16)
                geometryF%roughness = 0.7_wp
                z2Expected = 2.22043_wp
                qoExpected = 1.38699d-05
                zExpected  = 4.27801_wp
            case (17)
                modelFactors%m_z2                = 1.10_wp
                modelFactors%CriticalOvertopping = 0.9_wp
                z2Expected = 3.35616_wp
                qoExpected = 2.56622d-04
                zExpected  = 1.25479_wp
            case (18)
                modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
                modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
                z2Expected = 3.05105_wp
                qoExpected = 6.31188d-04
                zExpected  = 4.60152d-01
            case default    
                z2Expected = 0.00000_wp
                qoExpected = 0.00000d-06
                zExpected  = 0.00000_wp
        end select
            
!        z2String = achar(testNr)    
        write (z2String,"(A,I2)") "z2 from dllOvertopping.dll case " , testNr   
        write (qoString,"(A,I2)") "qo from dllOvertopping.dll case " , testNr   
        write (zString, "(A,I2)") "Z from dllOvertopping.dll case "  , testNr   
        !
        ! ===== actual test computations =====
        !
        call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
        call assert_true(succes, errorMessage)
        call assert_comparable(overtopping%z2, z2Expected, margin, z2String)
        call assert_comparable(overtopping%Qo, qoExpected, margin, qoString)

        call calcZValue(criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage) 
        call assert_true(succes, errorMessage)
        call assert_comparable(z, zExpected, margin, zString)
        
    end do
    !
    ! ===== clean up =====
    !
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingDllTest2

!> check output omkeervariant
subroutine TestOmkeervar
    integer                        :: npoints             ! number of coordinates
    real(kind=wp), parameter       :: dikeHeight = 0.2534_wp
    type(OvertoppingGeometryTypeF) :: geometryF
    type(tpOvertoppingInput)       :: modelFactors
    type(TErrorMessages)           :: errorStruct
    type (tpLoad)                  :: load           !< structure with load data
    type(tLogging)                 :: logging        !< logging struct
    type (tpOvertopping)           :: overtopping    !< structure with overtopping results
    logical                        :: succes
    character(len=128)             :: errorMessage      !< error message
    integer(kind=pntlen)           :: p

    pointer(qVal, ValidateInputF)
    pointer(qq  , calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qVal = getprocaddress (p, "ValidateInputF"C)
    qq   = getprocaddress (p, "calculateQoF"C)

    call init_modelfactors_and_load(modelFactors)

    npoints = 2
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    !
    geometryF%xcoords      = [-16.0_wp, 16.0_wp]
    geometryF%ycoords      = [ -4.0_wp,  4.0_wp]
    geometryF%roughness    = 1.0_wp
    geometryF%normal = 270.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! test actual computations in iterateToGivenDischarge; ...
    !
    ! water level below first lowest profile point
    load%h        =  -1.0_wp
    load%phi      = 270.0_wp
    load%Hm0      =   0.8_wp
    load%Tm_10    =   3.3_wp
 
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_true(succes, errorMessage)
!    call assert_comparable(overtopping%z2, z2Expected, margin, z2String)
    call assert_comparable(overtopping%Qo, 1.0d-3, 1.0d-4, 'test overslagdebiet')

end subroutine TestOmkeervar

!> test for issue Overs-52 (two berm segments)
!! @ingroup DikeOvertoppingTests
subroutine TestIssue52
    logical                        :: succes
    integer, parameter             :: npointsA = 5
    integer, parameter             :: npointsB = 4
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryA, geometryB
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging
    integer(kind=pntlen)           :: p
    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)

    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactors)
    load%h        =  3.0_wp
    load%phi      =  0.0_wp
    load%Hm0      =  2.0_wp
    load%Tm_10    =  5.65997611606080_wp

    allocate(geometryA%xcoords(npointsA), geometryA%ycoords(npointsA), geometryA%roughness(npointsA-1))
    geometryA%roughness = 1.0_wp
    geometryA%xcoords   = [0.0_wp, 24.15_wp, 28.15_wp, 32.15_wp, 40.55_wp]
    geometryA%ycoords   = [-2.0_wp, 3.75_wp,  4.0_wp, 4.0_wp, 6.0_wp]

    geometryA%normal = 0.0_wp ! degrees
    geometryA%npoints = npointsA
    
    allocate(geometryB%xcoords(npointsB), geometryB%ycoords(npointsB), geometryB%roughness(npointsB-1))
    geometryB%roughness = 1.0_wp
    geometryB%xcoords   = [0.0_wp, 24.15_wp, 32.15_wp, 40.55_wp]
    geometryB%ycoords   = [-2.0_wp, 3.75_wp, 4.0_wp, 6.0_wp]

    geometryB%normal = 0.0_wp ! degrees
    geometryB%npoints = npointsB
    !
    ! test actual computations in iterateToGivenDischarge
    !
    dikeHeight = 4.004_wp
    call calculateQoF(load, geometryA, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_comparable(overtopping%qo, overtopping%Qo, 0.236185_wp, 'discharge two berms, dikeHeight = 4.004_wp')

    dikeHeight = 4.007_wp
    call calculateQoF(load, geometryA, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_comparable(overtopping%qo, overtopping%Qo, 7.161912d-2, 'discharge two berms, dikeHeight = 4.007_wp')

    dikeHeight = 3.99_wp
    call calculateQoF(load, geometryA, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_comparable(overtopping%qo, overtopping%Qo, 0.242233_wp, 'discharge two berms, dikeHeight = 3.99_wp')

    dikeHeight = 4.004_wp
    call calculateQoF(load, geometryB, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_comparable(overtopping%qo, overtopping%Qo, 0.236185_wp, 'discharge one berm, dikeHeight = 4.004_wp')

    dikeHeight = 4.007_wp
    call calculateQoF(load, geometryB, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_comparable(overtopping%qo, overtopping%Qo, 7.161912d-2, 'discharge one berm, dikeHeight = 4.007_wp')

    dikeHeight = 3.99_wp
    call calculateQoF(load, geometryB, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging)
    call assert_comparable(overtopping%qo, overtopping%Qo,  0.242233_wp, 'discharge one berm, dikeHeight = 3.99_wp')


    ! clean up
    deallocate(geometryA%xcoords, geometryA%ycoords, geometryA%roughness)
    deallocate(geometryB%xcoords, geometryB%ycoords, geometryB%roughness)

end subroutine TestIssue52

end module dllTests
