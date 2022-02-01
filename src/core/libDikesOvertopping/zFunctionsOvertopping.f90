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
!! This file contains the limit state functions for wave overtopping within VTV
!<
!
! $Id$
!
!>
!! Module for the Limit State Functions (Z-functions) for wave overtopping
!! @ingroup dllOvertopping
!<
module zFunctionsOvertopping
    use precision,                   only : wp
    use overtoppingInterface,        only : tpProfileCoordinate, varModelFactorCriticalOvertopping
    use typeDefinitionsOvertopping,  only : xDiff_min, slope_min, tpGeometry, tpLoad, tpOvertoppingInput, tpOvertopping, &
                                            tpGeometries
    use mainModuleOvertopping,       only : calculateOvertopping, setupGeometries, cleanupGeometry
    use geometryModuleOvertopping,   only : initializeGeometry, deallocateGeometry
    use OvertoppingMessages
    use vectorUtilities,             only : interpolateLine
    use ModuleLogging
    use errorMessages
    use equalReals

    implicit none

    private

    public :: calculateQoHPC, zFuncLogRatios, profileInStructure

contains

!>
!! Subroutine to calculate the overtopping discharge with the Overtopping dll
!! @ingroup LibOvertopping
subroutine calculateQoHPC(dikeHeight, modelFactors, overtopping, load, geometries, logging, error)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQoHPC" :: calculateQoHPC
    real(kind=wp),                 intent(in)    :: dikeHeight     !< dike height
    type(tpOvertoppingInput),      intent(inout) :: modelFactors   !< struct with model factors
    type (tpOvertopping),          intent(out)   :: overtopping    !< structure with overtopping results
    type (tpGeometries), target,   intent(inout) :: geometries     !< structure with geometry data
    type (tpLoad),                 intent(in)    :: load           !< structure with load parameters
    type(tLogging),                intent(in)    :: logging        !< logging struct
    type(tMessage),                intent(inout) :: error          !< error struct

    integer                       :: nrCoordsAdjusted       !< number of coordinates of the adjusted profile
    type (tpGeometry), pointer    :: geometryAdjusted       !< structure for the adjusted profile
    type (tpGeometry), pointer    :: geometry               !< structure for the base profile

    ! ==========================================================================================================
    !
    !   Initialise
    !

    geometryAdjusted => geometries%adjWithDikeHeight
    geometry         => geometries%base

    call profileInStructure(geometry%nCoordinates, geometry%xcoordinates, geometry%ycoordinates, dikeHeight, &
                            nrCoordsAdjusted, geometries%xCoordsAdjusted, geometries%zCoordsAdjusted, error)

    if (error%errorCode == 0) then
        call initializeGeometry (geometry%psi, nrCoordsAdjusted, geometries%xCoordsAdjusted, geometries%zCoordsAdjusted, &
                                 geometry%roughnessFactors, geometryAdjusted, error)
    endif

    if (error%errorCode == 0) then
        ! first time we use load
        call calculateOvertopping (geometryAdjusted, load, modelFactors, overtopping, logging, error)
    endif

    if (error%errorCode /= 0) then
        overtopping%Qo = tiny(1.0d0)
        overtopping%z2 = 0d0
    endif

    geometries%geometryNoBerms(:)%nCoordinates = 0
end subroutine calculateQoHPC

!>
!! Subroutine to fill the profile in a structure and call the adjustment function of the profile due to a desired dike height
!! @ingroup LibOvertopping
subroutine profileInStructure(nrCoordinates, xcoordinates, ycoordinates, dikeHeight, nrCoordsAdjusted, &
                               xCoordsAdjusted, zCoordsAdjusted, error)
    integer,            intent(in)          :: nrCoordinates                !< number of coordinates of the profile
    real(kind=wp),      intent(in)          :: xcoordinates(nrCoordinates)  !< vector with x-coordinates of the profile
    real(kind=wp),      intent(in)          :: ycoordinates(nrCoordinates)  !< vector with y-coordinates of the profile
    real(kind=wp),      intent(in)          :: dikeHeight                   !< dike height
    integer,            intent(out)         :: nrCoordsAdjusted             !< number of coordinates in the adjusted profile
    real(kind=wp),      allocatable         :: xCoordsAdjusted(:)           !< vector with x-coordinates of the adjusted profile
    real(kind=wp),      allocatable         :: zCoordsAdjusted(:)           !< vector with y-coordinates of the adjusted profile
    type(tMessage),     intent(inout)       :: error                        !< error struct

    type(tpProfileCoordinate)     :: coordinates(nrCoordinates)             !< structure for the profile
    integer                       :: i                                      !< do-loop counter

    do i = 1, nrCoordinates
        coordinates(i)%xCoordinate = xcoordinates(i)
        coordinates(i)%zCoordinate = ycoordinates(i)
    enddo
    call adjustProfile(nrCoordinates, coordinates, dikeHeight, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, error)

end subroutine profileInStructure

!>
!! Subroutine adjust the profile due to a desired dike height
!! @ingroup LibOvertopping
subroutine adjustProfile(nrCoordinates, coordinates, dikeHeight, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, error)

    integer,                    intent(in)  :: nrCoordinates                !< number of coordinates of the profile
    type(tpProfileCoordinate),  intent(in)  :: coordinates(nrCoordinates)   !< structure for the profile
    real(kind=wp),              intent(in)  :: dikeHeight                   !< dike height
    integer,                    intent(out) :: nrCoordsAdjusted             !< number of coordinates in the adjusted profile
    real(kind=wp),              allocatable :: xCoordsAdjusted(:)           !< vector with x-coordinates of the adjusted profile
    real(kind=wp),              allocatable :: zCoordsAdjusted(:)           !< vector with y-coordinates of the adjusted profile
    type(tMessage),             intent(inout) :: error                      !< error struct

    ! locals
    !
    real(kind=wp)     :: auxiliaryHeightToe     !< auxiliary height toe of the profile
    real(kind=wp)     :: auxiliaryHeightBerm    !< auxiliary height berm of the profile
    real(kind=wp)     :: auxiliaryHeight        !< auxiliary height for the profile
    real(kind=wp)     :: slope                  !< slope of the profile
    integer           :: i                      !< do-loop counter
    logical           :: previousWasBerm        !< previous segment is a berm

    error%errorCode = 1
    if (nrCoordinates < 2) then
        call GetMSGdimension_cross_section_less_than_2(error%Message)
        return
    endif

    ! the minimal distance between two x-coordinates of the cross section is xDiff_min 
    ! compute the height of the cross section on the point xDiff_min meters after the toe

    auxiliaryHeightToe = interpolateLine(coordinates(1)%xCoordinate, coordinates(2)%xCoordinate, coordinates(1)%zCoordinate, coordinates(2)%zCoordinate, coordinates(1)%xCoordinate + xDiff_min, error)
    if (error%errorCode /= 0) return

    ! First the situation where the new dike height is close to the dike toe
    if (dikeHeight <= auxiliaryHeightToe) then
        nrCoordsAdjusted = 2
        call reallocAdjustedCoordinates(xCoordsAdjusted, zCoordsAdjusted, nrCoordsAdjusted, error)
        if (error%errorCode == 0) then
            zCoordsAdjusted(2) = dikeHeight
            xCoordsAdjusted(2) = interpolateLine(coordinates(1)%zCoordinate, coordinates(2)%zCoordinate, coordinates(1)%xCoordinate, coordinates(2)%xCoordinate, dikeHeight, error)
            if (error%errorCode /= 0) return
            xCoordsAdjusted(1) = xCoordsAdjusted(2) - xDiff_min
            zCoordsAdjusted(1) = interpolateLine(coordinates(1)%xCoordinate, coordinates(2)%xCoordinate, coordinates(1)%zCoordinate, coordinates(2)%zCoordinate, xCoordsAdjusted(1), error)
            if (error%errorCode /= 0) return
        else
            return
        endif
    else
        nrCoordsAdjusted = nrCoordinates
        previousWasBerm = .false.
        do i = 2, nrCoordinates - 1
            slope = (coordinates(i+1)%zCoordinate - coordinates(i  )%zCoordinate) / (coordinates(i+1)%xCoordinate - coordinates(i  )%xCoordinate)
            if (slope < slope_min .and. i < nrCoordinates - 1) then
                !
                ! the next segment of the cross section is a berm segment
                auxiliaryHeightBerm = interpolateLine(coordinates(i+1)%xCoordinate, coordinates(i+2)%xCoordinate, coordinates(i+1)%zCoordinate, coordinates(i+2)%zCoordinate, coordinates(i+1)%xCoordinate + xDiff_min, error)
                if (error%errorCode /= 0) return
                if (dikeHeight < auxiliaryHeightBerm) then
                    nrCoordsAdjusted = merge(i-1, i, previousWasBerm)
                    exit
                endif
            else
                auxiliaryHeight = interpolateLine(coordinates(i-1)%xCoordinate, coordinates(i)%xCoordinate, coordinates(i-1)%zCoordinate, coordinates(i)%zCoordinate, coordinates(i-1)%xCoordinate + xDiff_min, error)
                if (error%errorCode /= 0) return
                if (dikeHeight < auxiliaryHeight) then
                    nrCoordsAdjusted = i - 1
                    exit
                elseif (equalRealsRelative(dikeHeight, coordinates(i)%zCoordinate, 1d-12)) then
                    nrCoordsAdjusted = i
                    exit
                elseif (dikeHeight < coordinates(i)%zCoordinate) then
                    nrCoordsAdjusted = i
                    exit
                endif
            endif
            previousWasBerm = slope < slope_min
        enddo
        
        !
        ! allocate xCoordsAdjusted and zCoordsAdjusted and check result
        !
        call reallocAdjustedCoordinates(xCoordsAdjusted, zCoordsAdjusted, nrCoordsAdjusted, error)
        if (error%errorCode == 0) then

            ! all segments of the profile except the last
            do i = 1, nrCoordsAdjusted - 1
                xCoordsAdjusted(i) = coordinates(i)%xCoordinate
                zCoordsAdjusted(i) = coordinates(i)%zCoordinate
            enddo

            ! last segment of the profile
            zCoordsAdjusted(nrCoordsAdjusted) = dikeHeight
            xCoordsAdjusted(nrCoordsAdjusted) = interpolateLine(coordinates(nrCoordsAdjusted-1)%zCoordinate, coordinates(nrCoordsAdjusted)%zCoordinate, &
                coordinates(nrCoordsAdjusted-1)%xCoordinate, coordinates(nrCoordsAdjusted)%xCoordinate, dikeHeight, error)
            if (error%errorCode /= 0) return

            if (xCoordsAdjusted(nrCoordsAdjusted) < xCoordsAdjusted(nrCoordsAdjusted-1)) then
                call GetMSGadjusted_xcoordinates(error%Message)
                return
            endif
        else
            return
        endif
    endif
    error%errorCode = 0
end subroutine adjustProfile

subroutine reallocAdjustedCoordinates(xCoordsAdjusted, zCoordsAdjusted, nrCoordsAdjusted, error)
    real(kind=wp),    allocatable   :: xCoordsAdjusted(:), zCoordsAdjusted(:)
    integer,          intent(in)    :: nrCoordsAdjusted
    type(tMessage),   intent(inout) :: error

    error%errorCode = 0
    if (allocated(xCoordsAdjusted)) then
        if (size(xCoordsAdjusted) == nrCoordsAdjusted) then
            return
        endif
        deallocate(xCoordsAdjusted, zCoordsAdjusted)
    end if

    allocate(xCoordsAdjusted(nrCoordsAdjusted), zCoordsAdjusted(nrCoordsAdjusted), stat=error%errorCode)
    if (error%errorCode /= 0) then
        write(error%Message, GetFMTallocateError()) 2*nrCoordsAdjusted
    end if
end subroutine reallocAdjustedCoordinates

!>
!! Routine to compute the limit state value by using the logs of the overtopping discharges (computed and desired)
!! @ingroup LibOvertopping
function zFuncLogRatios(qo, qc, mqo, mqc, success, errorMessage) result (z)
    real (kind=wp),   intent(in)  :: qo            !< computed overtopping discharge
    real (kind=wp),   intent(in)  :: qc            !< Critical overtopping discharge
    real (kind=wp),   intent(in)  :: mqo           !< Model factor computed overtopping discharge
    real (kind=wp),   intent(in)  :: mqc           !< Model factor Critical overtopping discharge
    logical,          intent(out) :: success       !< Flag for succes
    character(len=*), intent(out) :: errorMessage  !< error message, only set if not successful
    real (kind=wp)                :: z             !< Value z-function
    !
    ! ==========================================================================================================
    !
    ! Notes:
    ! - The overtopping rates are an exponential function of several of its arguments
    !   Use the logarithm instead of the function itself to avoid convergence problems
    ! - Tiny overtopping rates should not lead to log(0)
    !
    if (mqc > 0.0d0 .and. qc > 0.0d0) then
        z = log(mqc * qc) - log(max(mqo * qo, tiny(1.0d0)))
        success = .true.
    else
        success = .false.
        if (mqc <= 0.0d0) then
            write(errorMessage, GetFMTzero_or_negative_varModelFactorCriticalOvertopping()) &
               varModelFactorCriticalOvertopping
        else ! qc <=0.0d0
            write(errorMessage, GetFMTzero_or_negative_critical_overtopping()) qc
        endif
    endif
end function ZFuncLogRatios

end module zFunctionsOvertopping
