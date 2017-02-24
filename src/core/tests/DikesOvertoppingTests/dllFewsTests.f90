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
!! Contains the module dllFewsTests of the DikesOvertopping dll
!
! $Id$
!
!> 
!! Module, holding tests of the functions of the dll.
!!
!! @ingroup DikeOvertoppingTests
module dllFewsTests
use precision, only : wp
use typeDefinitionsOvertopping
use testHelper, only : init_modelfactors_and_load
use ftnunit
use user32
use kernel32

implicit none

private

public :: allOvertoppingDllFewsTests

contains

subroutine allOvertoppingDllFewsTests
    call testWithLevel(overtoppingValidationFewsTest, 'Test validation for Fews (Java)', 1)
    call testWithLevel(overtoppingValidationFewsTest2, 'Test validation that passes for Fews (Java)', 1)
    call testWithLevel(TestCalculateQoJ, 'Test CalculateQoJ', 1)
end subroutine allOvertoppingDllFewsTests

!! @ingroup DikeOvertoppingTests
subroutine TestCalculateQoJ

    real(kind=wp), parameter :: zExpected1a =   11.725
    real(kind=wp), parameter :: zExpected1b =  701.48866_wp
    real(kind=wp), parameter :: zExpected2 = -6.43985_wp
    real(kind=wp), parameter :: margin     =  0.0000100_wp

    integer                        :: p
    external                       :: calculateQoJ
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    character(len=128)             :: errorMessage      !< error message
    type (tpLoad)                  :: loadStruct        !< structure with load data
    real(kind=wp)                  :: load(4)
    real(kind=wp)                  :: xcoords(npoints), ycoords(npoints), roughness(npoints)
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactorsStruct
    real(kind=wp)                  :: modelFactors(8)
    real(kind=wp)                  :: criticalOvertoppingRate
    real(kind=wp)                  :: normal
    real(kind=wp)                  :: output(2)

    pointer            (qc, calculateQoJ)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoJ"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactorsStruct, loadStruct)
    criticalOvertoppingRate        = 1.0d-3

    do i = 1, npoints
        xcoords(i)   = 5 * i
        ycoords(i)   = 3 + 2 * i
        if (i < npoints) roughness(i) = 1.0_wp
    enddo
    normal = 60.0_wp ! degrees
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheight
    !
    call convertJ(modelFactorsStruct, modelFactors, loadStruct, load)
    call calculateQoJ(load, xcoords, ycoords, roughness, normal, npoints, dikeHeight, modelFactors, output, succes, errorMessage)
    call assert_true(succes, errorMessage)
    call assert_comparable(output(1), 0.808902537811215d-8, margin, 'Qo from dllOvertopping.dll')
    call assert_comparable(output(2), 1.51985829057_wp, margin, 'z2 from dllOvertopping.dll')
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel > dikeheight (overflow)
    !
    loadStruct%h        =  9.50_wp
    call convertJ(modelFactorsStruct, modelFactors, loadStruct, load)
    call calculateQoJ(load, xcoords, ycoords, roughness, normal, npoints, dikeHeight, modelFactors, output, succes, errorMessage)
    call assert_false(succes, errorMessage)
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheight, without waves
    !
    loadStruct%h        =  5.50_wp
    loadStruct%Hm0      =  0.00_wp
    call convertJ(modelFactorsStruct, modelFactors, loadStruct, load)
    call calculateQoJ(load, xcoords, ycoords, roughness, normal, npoints, dikeHeight, modelFactors, output, succes, errorMessage)
    call assert_true(output(1) == 0d0, 'expect zero discharge')

end subroutine TestCalculateQoJ

!> Test the validation functions for FEWS in dllOvertopping.dll.
!!
!! @ingroup DikeOvertoppingTests
subroutine overtoppingValidationFewsTest
    integer                        :: p
    external                       :: ValidateInputJ, SetLanguage
    integer, parameter             :: npoints = 5
    real(kind=wp)                  :: xcoords(nPoints)
    real(kind=wp)                  :: ycoords(nPoints)
    real(kind=wp)                  :: roughness(nPoints-1)
    real(kind=wp)                  :: normal
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: modelFactorsArray(8)
    real(kind=wp)                  :: criticalOvertoppingRate
    integer                        :: loc
    logical                        :: success
    character(len=256)             :: errorMsg

    pointer            (qvalidate, ValidateInputJ)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qvalidate = getprocaddress (p, "ValidateInputJ"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactors)
    call convertJ(modelFactors, modelFactorsArray)
    criticalOvertoppingRate        = 1.0d-3

    xcoords = [ 0, 10, 20, 30, 40 ]
    ycoords = [-5, 0, -1, 4, 0]
    roughness = [ 0.5, 0.5, 0.5, 0.5 ]

    normal = 60.0_wp ! degrees
    !
    ! do validation of input (geometry not correct) :
    !
    call ValidateInputJ(xcoords, ycoords, roughness, normal, nPoints, dikeHeight, modelFactorsArray, success, errorMsg)
    call assert_false(success, "expect failure")
    loc = index(errorMsg, 'Verticale coordinaten mogen niet afnemen.   0.00 en   -1.00 doen dat wel.')
    call assert_equal(loc, 6, "error message validate 1")
    loc = index(errorMsg, 'Verticale coordinaten mogen niet afnemen.   4.00 en    0.00 doen dat wel.')
    call assert_equal(loc, 85, 'error message validate 2')

    !
    ! do validation of input (modelfactor m_z2 < 0) :
    !
    ycoords = [-5, 0, 5, 6, 7]
    modelFactorsArray(3)     = -1.00_wp
    call SetLanguage('UK')
    call ValidateInputJ(xcoords, ycoords, roughness, normal, nPoints, dikeHeight, modelFactorsArray, success, errorMsg)
    call assert_false(success, "expect failure")
    loc = index(errorMsg, 'Model factor 2% wave runup smaller than  0.000')
    call assert_equal(loc, 7, 'error message validate 3')

    !
    ! do validation of input (modelfactor foreshore < 0.3) :
    !
    modelFactorsArray(3)  = 1.00_wp
    modelFactorsArray(8)  = 0.25_wp
    call ValidateInputJ(xcoords, ycoords, roughness, normal, nPoints, dikeHeight, modelFactorsArray, success, errorMsg)
    call assert_false(success, "expect failure")
    loc = index(errorMsg, 'Model factor reduction factor foreshore not between  0.300 and  1.000')
    call assert_equal(loc, 7, 'error message validate 4')

end subroutine overtoppingValidationFewsTest

!> Test the validation functions for FEWS in dllOvertopping.dll.
!! simple case that passes
!!
!! @ingroup DikeOvertoppingTests
subroutine overtoppingValidationFewsTest2
    integer                        :: p
    external                       :: ValidateInputJ, SetLanguage
    integer, parameter             :: npoints = 2
    real(kind=wp)                  :: xcoords(nPoints)
    real(kind=wp)                  :: ycoords(nPoints)
    real(kind=wp)                  :: roughness(nPoints-1)
    real(kind=wp)                  :: normal
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: modelFactorsArray(8)
    real(kind=wp)                  :: criticalOvertoppingRate
    logical                        :: success
    character(len=256)             :: errorMsg

    pointer            (qvalidate, ValidateInputJ)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qvalidate = getprocaddress (p, "ValidateInputJ"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    call init_modelfactors_and_load(modelFactors)
    criticalOvertoppingRate        = 1.0d-3

    modelFactorsArray(1) = modelFactors%factorDeterminationQ_b_f_n
    modelFactorsArray(2) = modelFactors%factorDeterminationQ_b_f_b
    modelFactorsArray(3) = modelFactors%m_z2
    modelFactorsArray(4) = modelFactors%fshallow
    modelFactorsArray(5) = modelFactors%ComputedOvertopping
    modelFactorsArray(6) = modelFactors%CriticalOvertopping
    modelFactorsArray(7) = modelFactors%relaxationFactor
    modelFactorsArray(8) = modelFactors%reductionFactorForeshore

    xcoords = [ 0, 10 ]
    ycoords = [-5, 0]
    roughness = [ 0.5 ]

    normal = 60.0_wp ! degrees
    !
    ! do validation of input (geometry not correct) :
    !
    call ValidateInputJ(xcoords, ycoords, roughness, normal, nPoints, dikeHeight, modelFactorsArray, success, errorMsg)
    call assert_true(success, "expect failure")
end subroutine overtoppingValidationFewsTest2

subroutine convertJ(modelFactors, modelFactorsArray, load, loadArray)
    type(tpOvertoppingInput), intent(in) :: modelFactors
    real(kind=wp), intent(out) :: modelFactorsArray(:)
    type(tpLoad), intent(in), optional :: load
    real(kind=wp), intent(out), optional :: loadArray(:)

    modelFactorsArray(1) = modelFactors%factorDeterminationQ_b_f_n
    modelFactorsArray(2) = modelFactors%factorDeterminationQ_b_f_b
    modelFactorsArray(3) = modelFactors%m_z2
    modelFactorsArray(4) = modelFactors%fshallow
    modelFactorsArray(5) = modelFactors%ComputedOvertopping
    modelFactorsArray(6) = modelFactors%CriticalOvertopping
    modelFactorsArray(7) = modelFactors%relaxationFactor
    modelFactorsArray(8) = modelFactors%reductionFactorForeshore
    
    if (present(load) .and. present(loadArray)) then
        loadArray(1) = load%h    
        loadArray(2) = load%Hm0  
        loadArray(3) = load%Tm_10
        loadArray(4) = load%phi  
    endif

end subroutine convertJ

end module dllFewsTests
