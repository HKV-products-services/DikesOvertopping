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
    call testWithLevel(overtoppingValidationFewsTest,  'Java/FEWS interface; Test validation (A)', 1)
    call testWithLevel(overtoppingValidationFewsTest2, 'Java/FEWS interface; Test validation (B)', 1)
    call testWithLevel(TestCalculateQoJ,               'Java/FEWS interface; Test CalculateQoJ', 1)
    call testWithLevel(omkeerVariantTestJ,             'Java/FEWS interface; Test omkeerVariantJ', 1)
end subroutine allOvertoppingDllFewsTests

!! @ingroup DikeOvertoppingTests
subroutine TestCalculateQoJ
    integer                        :: p
    external                       :: calculateQoJ
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    character(len=256)             :: errorMessage      !< error message
    type (tpLoad)                  :: loadStruct        !< structure with load data
    real(kind=wp)                  :: load(4)
    real(kind=wp)                  :: xcoords(npoints)
    real(kind=wp)                  :: ycoords(npoints)
    real(kind=wp)                  :: roughness(npoints) = 1.0_wp
    real(kind=wp), parameter       :: dikeHeight = 9.1_wp
    type(tpOvertoppingInput)       :: modelFactorsStruct
    real(kind=wp)                  :: modelFactors(8)
    real(kind=wp)                  :: criticalOvertoppingRate
    real(kind=wp), parameter       :: normal  = 60.0_wp ! degrees
    real(kind=wp)                  :: output(2)
    real(kind=wp), parameter       :: margin     =  0.00001_wp


    pointer            (qc, calculateQoJ)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoJ"C)
    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactorsStruct, loadStruct)
    criticalOvertoppingRate        = 1.0d-3

    do i = 1, npoints
        xcoords(i)   = 5 * i
        ycoords(i)   = 3 + 2 * i
    enddo
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
    real(kind=wp)                  :: roughness(nPoints-1) = 0.5
    real(kind=wp), parameter       :: normal = 60.0_wp ! degrees
    real(kind=wp), parameter       :: dikeHeight = 9.1_wp
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: modelFactorsArray(8)
    real(kind=wp), parameter       :: criticalOvertoppingRate = 1.0d-3
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
    call init_modelfactors_and_load(modelFactors)
    call convertJ(modelFactors, modelFactorsArray)

    xcoords = [ 0, 10, 20, 30, 40 ]
    ycoords = [-5, 0, -1, 4, 0]
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
    real(kind=wp)                  :: roughness(nPoints-1) = 0.5
    real(kind=wp), parameter       :: normal = 60.0_wp ! degrees
    real(kind=wp), parameter       :: dikeHeight = 9.1_wp
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: modelFactorsArray(8)
    real(kind=wp), parameter       :: criticalOvertoppingRate  = 1.0d-3
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
    call init_modelfactors_and_load(modelFactors)
    call convertJ(modelFactors, modelFactorsArray)

    xcoords = [ 0, 10 ]
    ycoords = [-5, 0]

    !
    ! do validation of input (geometry not correct) :
    !
    call ValidateInputJ(xcoords, ycoords, roughness, normal, nPoints, dikeHeight, modelFactorsArray, success, errorMsg)
    call assert_true(success, "expect failure")
end subroutine overtoppingValidationFewsTest2

!> test for omkeerVariantJ
!! @ingroup DikeOvertoppingTests
subroutine omkeerVariantTestJ
    integer                        :: p
    external                       :: omkeerVariantJ
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    character(len=256)             :: errorMessage      !< error message
    type (tpLoad)                  :: loadStruct        !< structure with load data
    real(kind=wp)                  :: load(8)           !< structure with load data
    real(kind=wp)                  :: dikeHeight
    real(kind=wp), parameter       :: normal = 60.0_wp
    real(kind=wp)                  :: overtopping(2)
    real(kind=wp)                  :: xcoords(npoints)
    real(kind=wp)                  :: ycoords(npoints)
    real(kind=wp)                  :: roughness(npoints-1) = 1.0_wp
    type(tpOvertoppingInput)       :: modelFactorsStruct
    real(kind=wp)                  :: modelFactors(8)
    real(kind=wp), parameter       :: givenDischarge = 0.8d-8  !< discharge to iterate to

    pointer            (q, omkeerVariantJ)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    call assert_true(p /= 0, 'load dllDikesOvertopping.dll')
    q = getprocaddress (p, "omkeerVariantJ"C)
    call assert_true(q /= 0, 'get function pointer to omkeerVariantJ')
    if (q == 0) return
    !
    ! initializations
    !
    call init_modelfactors_and_load(modelFactorsStruct, loadStruct)

    do i = 1, npoints
        xcoords(i)   = 5 * i
        ycoords(i)   = 3 + 2 * i
    enddo
    !
    ! test actual computations in omkeerVariant for waterlevel < dikeheight
    !
    call convertJ(modelFactorsStruct, modelFactors, loadStruct, load)
    call omkeerVariantJ(load, xcoords, ycoords, roughness, normal, nPoints, givenDischarge, dikeHeight, modelFactors, overtopping, succes, errorMessage)
    call assert_true(succes, errorMessage)
    call assert_comparable(dikeHeight, 9.1d0, 1d-3, 'dikeHeight from omkeer variant')
    call assert_comparable(overtopping(1), 1.5_wp, 0.1_wp, 'z2 from omkeer variant')
    call assert_comparable(overtopping(2), givenDischarge, 1.5d-3, 'discharge last iteration from omkeer variant')

end subroutine omkeerVariantTestJ

!> helper function for conversion from Fortran datastructures to Java datastructures
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
