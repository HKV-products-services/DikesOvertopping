! Copyright (C) Stichting Deltares 2017. All rights reserved.
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
use precision, only : wp
use overtoppingInterface, only : OvertoppingGeometryTypeF
use typeDefinitionsOvertopping
use ModuleLogging
use waveParametersUtilities, only : computeWavePeriod
use zFunctionsOvertopping, only : profileInStructure
use testHelper, only : init_modelfactors_and_load
use ftnunit
use errorMessages
use user32
use kernel32

implicit none

private

public :: allOvertoppingDllTests

contains

subroutine allOvertoppingDllTests
    call testWithLevel(overtoppingDllTest, 'Test the external overtopping dll', 1)
    call testWithLevel(overtoppingDikeInProfileTest, 'Test a dikeheight at one of the profile points', 1)
    call testWithLevel(influenceRoughnessTest, 'Test influence roughness', 1)
    call testWithLevel(overtoppingValidationTest, 'Test validation of incorrect profile and negative model factor', 1)
    call testWithLevel(overtoppingValidationRoughnessTest, 'Test validation of invalid roughness', 1)
    call testWithLevel(overtoppingMultipleValidationTest, 'Test validation of incorrect profile and negative model factor in one call', 1)
    call testWithLevel(overtoppingValidationTestZPoints, 'Test message of incorrect profile (z-value)', 1)
    call testWithLevel(overtoppingZ2Test, 'Test h+z2 > dikeheight', 1)
    call testWithLevel(LoadNaNTest, 'Test error handling in case of NaN in load', 1)
    call testWithLevel(TestProfileAdjustment, "Test whether the profile is adapted correctly", 1)
    call testWithLevel(TestRoughnessIssue44A, "First test for calculateGammaF related to issue 44", 1)
    call testWithLevel(TestRoughnessIssue44B, "Second test for calculateGammaF related to issue 44", 1)
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

    integer                        :: p
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
    integer                        :: p
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

    integer                        :: p
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
    integer                        :: p
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
    integer                        :: p
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

!> Test the functions in dllOvertopping.dll.
!!     these functions are:
!!     - calcZValue
!!     - calculateQoF
!!     - versionNumber
!!
!!     - test overflow (waterlevel > dike height)
!!     - test with and without waves
!!
!! @ingroup DikeOvertoppingTests
subroutine overtoppingValidationTest
    integer                        :: p
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
    
    call assert_equal(errorStruct%messages(3)%message, 'Model factor 2% wave runup smaller than  0.000', 'error handling')
    
    !
    ! do validation of input (modelfactor foreshore < 0.3) :
    !
    modelFactors%m_z2                      = 1.00_wp
    modelFactors%reductionFactorForeshore  = 0.25_wp
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call assert_equal(errorStruct%messages(4)%message, 'Model factor reduction factor foreshore not between  0.300 and  1.000', 'error handling')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingValidationTest

subroutine overtoppingValidationRoughnessTest
    integer                        :: p
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
    integer                        :: p
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
    integer                        :: p
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
    geometryF%roughness = [ 1.0, 1.0, 1.0 ]
    
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
    geometryF%ycoords = [ 0d0, 9.9d0, 10d0]
    call SetLanguage('NL')
    call ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
    call assert_equal(4, errorStruct%nErrors, 'expected 4 (total) validation errors')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingValidationTestZPoints

!> test for the adjustment of the cross section
!!
!! @ingroup DikeOvertoppingTests
subroutine TestProfileAdjustment
!
!   Local parameters
!
    integer                    :: nCoordinates         ! number of coordinates
    integer                    :: nrCoordsAdjusted     ! number of coordinates
    real(kind=wp), allocatable :: xCoordinates(:)      ! x-coordinates (m)
    real(kind=wp), allocatable :: yCoordinates(:)      ! y-coordinates (m+NAP)
    real(kind=wp), pointer     :: xCoordsAdjusted(:)   ! vector with x-coordinates of the adjusted profile
    real(kind=wp), pointer     :: zCoordsAdjusted(:)   ! vector with y-coordinates of the adjusted profile
    logical                    :: succes               ! flag for succes
    character(len=255)         :: errorMessage         ! error message
    real(kind=wp)              :: dikeHeight           ! vector with x-coordinates of the adjusted profile
    integer                    :: i                    ! do-loop counter
    
    nCoordinates = 4
    allocate (xCoordinates (nCoordinates))
    allocate (yCoordinates (nCoordinates))
    
    xCoordinates(1) = 28.5d0
    xCoordinates(2) = 30.55d0
    xCoordinates(3) = 34.17d0
    xCoordinates(4) = 37.0d0
    
    yCoordinates(1) = 0.02d0
    yCoordinates(2) = 0.36d0
    yCoordinates(3) = 1.12d0
    yCoordinates(4) = 1.74d0
    
    dikeHeight = 0.74d0
    
    call profileInStructure(nCoordinates, xcoordinates, ycoordinates, dikeHeight, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, succes, errorMessage)
    
    do i = 2, nrCoordsAdjusted
        call assert_true (xCoordsAdjusted(i) > xCoordsAdjusted(i-1), "X coordinates increasing")
    enddo

    deallocate(xCoordinates)
    deallocate(yCoordinates)

    deallocate(xCoordsAdjusted)
    deallocate(zCoordsAdjusted)

end subroutine TestProfileAdjustment

!> basic test related to issue 44 (this situation went wrong)
!! @ingroup DikeOvertoppingTests
subroutine TestRoughnessIssue44A
!
!   Local parameters
!
    use dllOvertopping
    integer                        :: npoints        ! number of coordinates
    logical                        :: succes         ! flag for succes
    character(len=255)             :: errorMessage   ! error message
    real(kind=wp)                  :: dikeHeight     ! vector with x-coordinates of the adjusted profile
    type (tpLoad)                  :: load           !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging        !< logging struct
    type (tpOvertopping)           :: overtopping    !< structure with overtopping results

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
    use dllOvertopping
    integer                        :: npoints        ! number of coordinates
    logical                        :: succes         ! flag for succes
    character(len=255)             :: errorMessage   ! error message
    real(kind=wp)                  :: dikeHeight     ! vector with x-coordinates of the adjusted profile
    type (tpLoad)                  :: load           !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging        !< logging struct
    type (tpOvertopping)           :: overtopping    !< structure with overtopping results

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

end module dllTests
