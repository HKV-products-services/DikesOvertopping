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

!> @file dllOvertopping.f90
!!  Main entry for the dll DikesOvertopping
!!  FUNCTIONS/SUBROUTINES exported from dllOvertopping.dll:
!!  - calcZValue
!!  - calculateQo
!!  - calculateQoF
!!  - calculateQoJ
!!  - ValidateInputC
!!  - ValidateInputF
!!  - ValidateInputJ
!!  - omkeerVariantF
!!  - omkeerVariantJ
!!  - SetLanguage
!!  - GetLanguage
!!  - versionNumber
!
! $Id$
!
!
!> Main entry for the dll DikesOvertopping
!
module dllOvertopping
    use zFunctionsOvertopping,      only : calculateQoRTO, zFuncLogRatios
    use geometryModuleOvertopping,  only : deallocateGeometry
    use precision,                  only : wp
    use typeDefinitionsOvertopping, only : tpGeometry, tpLoad, tpOvertoppingInput
    use overtoppingInterface,       only : OvertoppingGeometryType, OvertoppingGeometryTypeF
    use, intrinsic :: iso_c_binding

    implicit none

    private

    !  FUNCTIONS/SUBROUTINES exported from dllOvertoppping.dll:
    public :: calculateQo, calculateQoF, calcZValue, versionNumber, ValidateInputC, ValidateInputF, &
              omkeerVariantF, setLanguage, getLanguage, &
              calculateQoJ, ValidateInputJ, omkeerVariantJ

    character, parameter :: separationChar = char(9)      !< use horizontal tab for separation

contains

!>
!! Subroutine that calculates the discharge needed for the Z-function DikesOvertopping
!! Wrapper for calculateQoF: convert C-like input structures to Fortran input structures
!!
!! @ingroup dllDikesOvertopping
subroutine calculateQo(load, geometryInput, dikeHeight, modelFactors, overtopping, success, errorText, verbosity, logFile)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQo" :: calculateQo
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use ModuleLogging
    type(OvertoppingGeometryType), intent(in) :: geometryInput  !< struct with geometry and roughness as c-pointers
    type(tpLoad), intent(in)                  :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                 :: dikeHeight     !< dike height
    type(tpOvertoppingInput), intent(inout)   :: modelFactors   !< struct with modelfactors
    type (tpOvertopping), intent(out)         :: overtopping    !< structure with overtopping results
    logical, intent(out)                      :: success        !< flag for success
    character(len=*), intent(out)             :: errorText      !< error message (only set if not successful)
    integer, intent(in)                       :: verbosity      !< level of verbosity
    character(len=*), intent(in)              :: logFile        !< filename of logfile

    type(OvertoppingGeometryTypeF)            :: geometry       !< fortran struct with geometry and roughness
    type(tLogging)                            :: logging        !< logging struct

    geometry = geometry_c_f(geometryInput)

    logging%verbosity = verbosity
    logging%filename = logFile

    call calculateQoF(load, geometry, dikeHeight, modelFactors, overtopping, success, errorText, logging)

end subroutine calculateQo

!>
!! Subroutine that calculates the discharge
!! Wrapper for calculateQoF: convert basic types to Fortran input structures
!!
!! @ingroup dllDikesOvertopping
subroutine calculateQoJ(load, xcoords, ycoords, roughness, normal, npoints, dikeHeight, modelFactors, output, succes, errorMessage)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQoJ" :: calculateQoJ
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use ModuleLogging
    integer, intent(in)                        :: npoints               !< the number of coordinates
    real(kind=wp), intent(in)                  :: dikeHeight            !< dike height
    real(kind=wp), intent(in)                  :: normal                !< dike normal
    logical, intent(out)                       :: succes                !< flag for success
    character(len=256), intent(out)            :: errorMessage          !< error message (only set if not successful)
    real(kind=wp), intent(in)                  :: load(4)               !< input load (wl and 3 wave numbers)
    real(kind=wp), intent(in)                  :: xcoords(npoints)      !< the x coordinates
    real(kind=wp), intent(in)                  :: ycoords(npoints)      !< the y coordinates
    real(kind=wp), intent(in)                  :: roughness(npoints-1)  !< the roughness at sections
    real(kind=wp), intent(in)                  :: modelFactors(8)       !< the overtopping modelfactors
    real(kind=wp), intent(out)                 :: output(2)             !< output array: 1: Qo; 2: Z2

    type(OvertoppingGeometryTypeF) :: geometryF      !< struct with geometry and roughness
    type(tpLoad)                   :: loadF          !< struct with waterlevel and wave parameters
    type(tpOvertoppingInput)       :: modelFactorsF  !< struct with modelFactors
    type (tpOvertopping)           :: overtopping    !< structure with overtopping results
    type(tLogging)                 :: logging        !< logging struct
    integer                        :: ierr           !< error code of allocate
    
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1), stat=ierr)
    if (ierr == 0) then
        call input_j_f(xcoords, ycoords, roughness, normal, geometryF, modelFactors, modelFactorsF, load, loadF)
        call calculateQoF(loadF, geometryF, dikeHeight, modelFactorsF, overtopping, succes, errorMessage, logging)
        output(1) = overtopping%Qo
        output(2) = overtopping%z2
    else
        succes = .false.
        call set_nan(output(1))
        call set_nan(output(2))
        errorMessage = 'Allocation error in calculateQoJ'
    endif
end subroutine calculateQoJ
!>
!! Subroutine that calculates the discharge needed for the Z-function DikesOvertopping
!!
!! @ingroup dllDikesOvertopping
subroutine calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, success, errorText, logging)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQoF" :: calculateQoF
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use ModuleLogging
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
    type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                  :: dikeHeight     !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
    type (tpOvertopping), intent(out)          :: overtopping    !< structure with overtopping results
    logical, intent(out)                       :: success        !< flag for success
    character(len=*), intent(out)              :: errorText      !< error message (only set if not successful)
    type(tLogging), intent(in)                 :: logging        !< logging struct
!
    type (tpGeometry)                          :: geometry       !< structure with geometry data
!
    call initializeGeometry(geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, &
                             geometryF%roughness, geometry, success, errorText)

    if (success) then
        call calculateQoRTO(dikeHeight, modelFactors, overtopping, load, geometry, logging, success, errorText)
    endif

    call deallocateGeometry(geometry)
end subroutine calculateQoF

!>
!! Subroutine that calculates the Z-function DikesOvertopping
!! based on the discharge calculated with calculateQoF
!!
!! @ingroup dllDikesOvertopping
subroutine calcZValue(criticalOvertoppingRate, modelFactors, Qo, z, success, errorMessage)
! use alias, otherwise the name will be dllOvertopping_mp_calcZValue
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calcZValue" :: calcZValue
    real(kind=wp), intent(in)               :: criticalOvertoppingRate    !< critical overtoppingrate
    type(tpOvertoppingInput), intent(inout) :: modelFactors               !< struct with modelfactors
    real(kind=wp), intent(in)               :: Qo                         !< calculated discharge
    real(kind=wp), intent(out)              :: z                          !< z value
    character(len=*), intent(out)           :: errorMessage               !< error message (only if not successful)
    logical, intent(out)                    :: success                    !< flag for success

    !==============================================================================
    z = zFuncLogRatios(qo, criticalOvertoppingRate, modelFactors%ComputedOvertopping, modelFactors%CriticalOvertopping, success, errorMessage)

end subroutine calcZValue

!>
!! Subroutine that validates the geometry
!! Wrapper for ValidateInputF: convert C-like input structures to Fortran input structures
!!
!! @ingroup dllDikesOvertopping
subroutine ValidateInputC(geometryInput, dikeHeight, modelFactors, success, errorText)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"ValidateInputC" :: ValidateInputC
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use errorMessages
    use OvertoppingMessages
    type(OvertoppingGeometryType), intent(in) :: geometryInput                 !< struct with geometry and roughness as c-pointers
    real(kind=wp), intent(in)                 :: dikeHeight                    !< dike height
    type(tpOvertoppingInput), intent(inout)   :: modelFactors                  !< struct with modelfactors
    logical, intent(out)                      :: success                       !< flag for success
    character(len=*), intent(out)             :: errorText                     !< error message (only set if not successful)

    type(OvertoppingGeometryTypeF)            :: geometry                      !< fortran struct with geometry and roughness
    type(TErrorMessages)                      :: errorStruct
    integer                                   :: i
    integer                                   :: nMessages
    character(len=8)                          :: msgtype

    geometry = geometry_c_f(geometryInput)

    call initErrorMessages(errorStruct)

    call ValidateInputF(geometry, dikeHeight, modelFactors, errorStruct)

    nMessages = errorStruct%nErrors + errorStruct%nWarnings
    success = nMessages == 0
    if (success) then
        errorText = ' '
    else
        do i = 1, nMessages
            if (errorStruct%messages(i)%severity == severityError) then
                msgtype = GetOvertoppingMessage(errorIndicator)
            else
                msgtype = GetOvertoppingMessage(warningIndicator)
            endif

            if (i == 1) then
                errorText = trim(msgtype) // ':' // errorStruct%messages(i)%message
            else
                errorText = trim(errorText) // separationChar // trim(msgtype) // ':' // errorStruct%messages(i)%message
            endif
        enddo
    endif

end subroutine ValidateInputC

!>
!! Subroutine that validates the geometry
!! Wrapper for ValidateInputF: convert basic types to Fortran input structures
!!
!! @ingroup dllDikesOvertopping
subroutine ValidateInputJ(x, y, roughness, normal, nPoints, dikeHeight, modelFactorsArray, success, errorText)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"ValidateInputJ" :: ValidateInputJ
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use errorMessages
    use OvertoppingMessages
    integer, intent(in)                       :: nPoints                       !< number of profile points
    real(kind=wp), intent(in)                 :: x(nPoints)                    !< x coordinates of profile
    real(kind=wp), intent(in)                 :: y(nPoints)                    !< y coordinates of profile
    real(kind=wp), intent(in)                 :: roughness(nPoints-1)          !< roughness of profile sections
    real(kind=wp), intent(in)                 :: normal                        !< dike normal
    real(kind=wp), intent(in)                 :: dikeHeight                    !< dike height
    real(kind=wp), intent(in)                 :: modelFactorsArray(8)          !< array with modelfactors
    logical, intent(out)                      :: success                       !< flag for success
    character(len=256), intent(out)           :: errorText                     !< error message (only set if not successful)

    type(tpOvertoppingInput)                  :: modelFactors                  !< struct with modelfactors
    type(OvertoppingGeometryTypeF)            :: geometry                      !< fortran struct with geometry and roughness
    type(TErrorMessages)                      :: errorStruct
    integer                                   :: i
    integer                                   :: ierr
    integer                                   :: nMessages
    character(len=8)                          :: msgtype

    allocate(geometry%xCoords(nPoints), geometry%yCoords(nPoints), geometry%roughness(nPoints-1), stat=ierr)
    if (ierr /= 0) then
        write(errorText,*) 'memory allocation error in ValidateInputJ with size: ', nPoints
        success = .false.
    else
        call input_j_f(x, y, roughness, normal, geometry, modelFactorsArray, modelFactors)
        call initErrorMessages(errorStruct)

        call ValidateInputF(geometry, dikeHeight, modelFactors, errorStruct)

        nMessages = errorStruct%nErrors + errorStruct%nWarnings
        success = nMessages == 0
        if (success) then
            errorText = ' '
        else
            do i = 1, nMessages
                if (errorStruct%messages(i)%severity == severityError) then
                    msgtype = GetOvertoppingMessage(errorIndicator)
                else
                    msgtype = GetOvertoppingMessage(warningIndicator)
                endif

                if (i == 1) then
                    errorText = trim(msgtype) // ':' // errorStruct%messages(i)%message
                else
                    errorText = trim(errorText) // separationChar // trim(msgtype) // ':' // errorStruct%messages(i)%message
                endif
            enddo
        endif
        deallocate(geometry%xCoords, geometry%yCoords, geometry%roughness)
    endif

end subroutine ValidateInputJ

!>
!! Subroutine that validates the geometry
!!
!! @ingroup dllDikesOvertopping
subroutine ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"ValidateInputF" :: ValidateInputF
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use zFunctionsOvertopping
    use mainModuleOvertopping, only : checkModelFactors
    use errorMessages
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF           !< struct with geometry and roughness
    real(kind=wp), intent(in)                  :: dikeHeight          !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors        !< struct with modelFactors
    type(TErrorMessages), intent(inout)        :: errorStruct         !< error message (only set if not successful)
!
!   locals
!
    type (tpGeometry)                          :: geometry            !< structure with geometry data
    integer                                    :: nrCoordsAdjusted    !< number of coordinates of the adjusted profile
    real(kind=wp), pointer                     :: xCoordsAdjusted(:)  !< vector with x-coordinates of the adjusted profile
    real(kind=wp), pointer                     :: zCoordsAdjusted(:)  !< vector with y-coordinates of the adjusted profile
    type (tpGeometry)                          :: geometryAdjusted    !< structure for the adjusted profile
    character(len=StrLenMessages)              :: errorText           !< local error or validation message
    integer, parameter                         :: maxErr = 32         !< max. number of validation messages
    character(len=StrLenMessages)              :: errorTexts(maxErr)  !< local error or validation messages
    logical                                    :: success             !< local error flag
    integer                                    :: ierr                !< actual number of validation messages
    type (tMessage)                            :: msgStruct           !< struct for one local error or validation message
    integer                                    :: i                   !< loop counter
!
    success = .true.
    errorText = ' '

    nullify(xCoordsAdjusted)
    nullify(zCoordsAdjusted)

    if (success) then
        call basicGeometryTest(geometryF, success, errorStruct)
    endif

    if (success) then
        call initializeGeometry (geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, &
                             geometryF%roughness, geometry, success, errorText)
    endif

    if (success) then

        call profileInStructure(geometry%nCoordinates, geometry%xcoordinates, geometry%ycoordinates, dikeHeight, &
                            nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, success, errorText)
    endif

    if (success) then
        call initializeGeometry(geometry%psi, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, &
                                geometry%roughnessFactors, geometryAdjusted, success, errorText)
        call deallocateGeometry(geometryAdjusted)
        call deallocateGeometry(geometry)
    endif

    if (.not. success) then
        if (errorText /= ' ') then
            msgStruct%errorCode = 1
            msgStruct%severity  = severityError
            msgStruct%message   = errorText
            call addMessage(errorStruct, msgStruct)
        endif
    endif

    call checkModelFactors (modelFactors, maxErr, errorTexts, ierr)
    do i = 1, ierr
       msgStruct%errorCode = 2
       msgStruct%severity  = severityError
       msgStruct%message   = errorTexts(i)
       call addMessage(errorStruct, msgStruct)
    enddo

    if (associated(xCoordsAdjusted)) deallocate(xCoordsAdjusted)
    if (associated(zCoordsAdjusted)) deallocate(zCoordsAdjusted)
end subroutine ValidateInputF

!>
!! Wrapper for omkeerVariantF: get dikeHeight by given discharge
!!
!! @ingroup dllDikesOvertopping
subroutine omkeerVariantJ(load, xcoords, ycoords, roughness, normal, npoints, givenDischarge, dikeHeight, modelFactors, output, succes, errorMessage)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"omkeerVariantJ" :: omkeerVariantJ
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use ModuleLogging
    integer, intent(in)                        :: npoints              !< number of coordinates
    real(kind=wp), intent(in)                  :: givenDischarge       !< input discharge
    real(kind=wp), intent(out)                 :: dikeHeight           !< dike height
    real(kind=wp), intent(in)                  :: normal               !< dike normal
    logical, intent(out)                       :: succes               !< flag for success
    character(len=256), intent(out)            :: errorMessage         !< error message (only set if not successful)
    real(kind=wp), intent(in)                  :: load(4)              !< input load (wl, and 3 wave parameters)
    real(kind=wp), intent(in)                  :: xcoords(npoints)     !< the x-coordinates
    real(kind=wp), intent(in)                  :: ycoords(npoints)     !< the y-coordinates
    real(kind=wp), intent(in)                  :: roughness(npoints-1) !< the roughness at sections
    real(kind=wp), intent(in)                  :: modelFactors(8)      !< the overtopping modelfactors
    real(kind=wp), intent(out)                 :: output(2)            !< output array: 1: Z2; 2: Qo

    type(OvertoppingGeometryTypeF) :: geometryF      !< struct with geometry and roughness
    type(tpLoad)                   :: loadF          !< struct with waterlevel and wave parameters
    type(tpOvertoppingInput)       :: modelFactorsF  !< struct with modelFactors
    type(tpOvertopping)            :: overtopping    !< structure with overtopping results
    type(tLogging)                 :: logging        !< logging struct
    integer                        :: ierr           !< error code of allocate

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1), stat=ierr)
    if (ierr == 0) then
        call input_j_f(xcoords, ycoords, roughness, normal, geometryF, modelFactors, modelFactorsF, load, loadF)
        call omkeerVariantF(loadF, geometryF, givenDischarge, dikeHeight, modelFactorsF, overtopping, succes, errorMessage, logging)
        output(1) = overtopping%z2
        output(2) = overtopping%Qo
    else
        succes = .false.
        call set_nan(output(1))
        call set_nan(output(2))
        errorMessage = 'Allocation error in omkeerVariantJ'
    endif
end subroutine omkeerVariantJ

!>
!! Subroutine with omkeerVariant (get dikeHeight by given discharge)
!!
!! @ingroup dllDikesOvertopping
subroutine omkeerVariantF(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText, logging)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"omkeerVariantF" :: omkeerVariantF
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use ModuleLogging
    use omkeerVariantModule
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
    type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                  :: givenDischarge !< discharge to iterate to
    real(kind=wp), intent(out)                 :: dikeHeight     !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
    type (tpOvertopping), intent(inout)        :: overtopping    !< structure with overtopping results
    logical, intent(out)                       :: success        !< flag for success
    character(len=*), intent(out)              :: errorText      !< error message (only set if not successful)
    type(tLogging), intent(in)                 :: logging        !< logging struct
!
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText, logging)
end subroutine omkeerVariantF

!>
!! Subroutine that sets the language for error and validation messages
!!
!! @ingroup dllDikesOvertopping
subroutine SetLanguage(lang)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SetLanguage" :: SetLanguage
use OvertoppingMessages, only : SetLanguageCore => SetLanguage
character(len=*), intent(in) :: lang

call SetLanguageCore(lang)

end subroutine SetLanguage

!>
!! Subroutine that gets the language for error and validation messages
!!
!! @ingroup dllDikesOvertopping
subroutine GetLanguage(lang)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"GetLanguage" :: GetLanguage
use OvertoppingMessages, only : GetLanguageCore => GetLanguage
character(len=*), intent(out) :: lang

call GetLanguageCore(lang)
end subroutine GetLanguage

!>
!! Subroutine that delivers the version number
!!
!! @ingroup dllDikesOvertopping
subroutine versionNumber(version)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"versionNumber" :: versionNumber
    character(len=*), intent(out) :: version !< version number
    !
    ! locals
    !
    character(len=*), parameter :: cversion = "16.2.3.5003"
    !
    !==============================================================================
    !
    if (len(version) >= len(cversion)) then
        version = cversion
    else
        version = repeat("*", len(version))
    endif

end subroutine versionNumber

!>
!! Private subroutine that converts geometry from c-pointer to fortran struct
!!
!! @ingroup dllDikesOvertopping
function geometry_c_f(geometryInput) result(geometry)
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping

    type(OvertoppingGeometryType), intent(in) :: geometryInput  !< struct with geometry and roughness as c-pointers
    type(OvertoppingGeometryTypeF)            :: geometry       !< fortran struct with geometry and roughness

    integer                                   :: n(1)           !< dimension definition in c_f_pointer call

    n(1) = geometryInput%nPoints

    call c_f_pointer(geometryInput%xCoords, geometry%xcoords, n)
    call c_f_pointer(geometryInput%ycoords, geometry%ycoords, n)

    n(1) = geometryInput%nPoints - 1
    call c_f_pointer(geometryInput%roughness, geometry%roughness, n)

    geometry%npoints = geometryInput%nPoints
    geometry%normal = geometryInput%normal

end function geometry_c_f

!> convert java input to Fortran input
subroutine input_j_f(x, y, roughness, normal, geometryF, modelFactorsJ, modelFactorsF, loadJ, loadF)
    real(kind=wp), intent(in) :: x(:), y(:), roughness(:), normal, modelFactorsJ(:)
    real(kind=wp), intent(in), optional :: loadJ(:)
    type(tpOvertoppingInput), intent(out) :: modelFactorsF
    type(OvertoppingGeometryTypeF), intent(out) :: geometryF
    type(tpLoad), intent(out), optional :: loadF

    geometryF%nPoints = size(x)
    geometryF%xCoords = x
    geometryF%yCoords = y
    geometryF%roughness = roughness
    geometryF%normal = normal

    modelFactorsF%factorDeterminationQ_b_f_n = modelFactorsJ(1)
    modelFactorsF%factorDeterminationQ_b_f_b = modelFactorsJ(2)
    modelFactorsF%m_z2                       = modelFactorsJ(3)
    modelFactorsF%fshallow                   = modelFactorsJ(4)
    modelFactorsF%ComputedOvertopping        = modelFactorsJ(5)
    modelFactorsF%CriticalOvertopping        = modelFactorsJ(6)
    modelFactorsF%relaxationFactor           = modelFactorsJ(7)
    modelFactorsF%reductionFactorForeshore   = modelFactorsJ(8)

    if (present(loadJ) .and. present(loadF)) then
        loadF%h     = loadJ(1)
        loadF%Hm0   = loadJ(2)
        loadF%Tm_10 = loadJ(3)
        loadF%phi   = loadJ(4)
    endif
end subroutine input_j_f

end module dllOvertopping
