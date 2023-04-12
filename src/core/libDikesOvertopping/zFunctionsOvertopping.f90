! Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
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
    use overtoppingInterface,        only : varModelFactorCriticalOvertopping
    use typeDefinitionsOvertopping,  only : tpGeometry, tpLoad, tpOvertoppingInput, tpOvertopping, tpGeometries, tpCoordinatePair
    use parametersOvertopping,       only : slope_min, xdiff_min
    use mainModuleOvertopping,       only : calculateOvertopping, setupGeometries, cleanupGeometry
    use geometryModuleOvertopping,   only : initializeGeometry, deallocateGeometry, allocCoordinatePair, copyCoordinates
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
subroutine calculateQoHPC(dikeHeight, modelFactors, overtopping, load, geometries, error)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQoHPC" :: calculateQoHPC
    real(kind=wp),                 intent(in)    :: dikeHeight     !< dike height
    type(tpOvertoppingInput),      intent(inout) :: modelFactors   !< struct with model factors
    type (tpOvertopping),          intent(out)   :: overtopping    !< structure with overtopping results
    type (tpGeometries), target,   intent(inout) :: geometries     !< structure with geometry data
    type (tpLoad),                 intent(in)    :: load           !< structure with load parameters
    type(tMessage),                intent(inout) :: error          !< error struct

    type (tpGeometry), pointer    :: geometryAdjusted       !< structure for the adjusted profile
    type (tpGeometry), pointer    :: geometry               !< structure for the base profile

    ! ==========================================================================================================
    !
    !   Initialise
    !

    geometryAdjusted => geometries%adjWithDikeHeight
    geometry         => geometries%base

    call profileInStructure(geometry%Coordinates, dikeHeight, geometries%CoordsAdjusted, error)

    if (error%errorCode == 0) then
        call initializeGeometry (geometry%psi, geometries%CoordsAdjusted, &
                                 geometry%roughnessFactors, geometryAdjusted, error)
    endif

    if (error%errorCode == 0) then
        ! first time we use load
        call calculateOvertopping (geometryAdjusted, load, modelFactors, overtopping, error)
    endif

    if (error%errorCode /= 0) then
        overtopping%Qo = tiny(1.0d0)
        overtopping%z2 = 0d0
    endif

    geometries%geometrySectionBNoBerms%Coordinates%N = 0
    geometries%geometrySectionFNoBerms%Coordinates%N = 0
end subroutine calculateQoHPC

!>
!! Subroutine to fill the profile in a structure and call the adjustment function of the profile due to a desired dike height
!! @ingroup LibOvertopping
subroutine profileInStructure(coordinates, dikeHeight, coordsAdjusted, error)
    type(tpCoordinatePair),  intent(in)     :: coordinates     !< structure for the profile
    real(kind=wp),           intent(in)     :: dikeHeight      !< dike height
    type(tMessage),          intent(inout)  :: error           !< error struct
    type(tpCoordinatePair),  intent(inout)  :: coordsAdjusted  !< coordinates in the adjusted profile

    ! locals
    !
    real(kind=wp)     :: auxiliaryHeightToe     !< auxiliary height toe of the profile
    real(kind=wp)     :: auxiliaryHeightBerm    !< auxiliary height berm of the profile
    real(kind=wp)     :: auxiliaryHeight        !< auxiliary height for the profile
    real(kind=wp)     :: slope                  !< slope of the profile
    integer           :: i                      !< do-loop counter
    logical           :: previousWasBerm        !< previous segment is a berm

    error%errorCode = 1
    if (Coordinates%N < 2) then
        call GetMSGdimension_cross_section_less_than_2(error%Message)
        return
    endif

    ! the minimal distance between two x-coordinates of the cross section is xDiff_min
    ! compute the height of the cross section on the point xDiff_min meters after the toe

    auxiliaryHeightToe = interpolateLine(coordinates%x(1), coordinates%x(2), coordinates%y(1), coordinates%y(2), coordinates%x(1) + xDiff_min, error)
    if (error%errorCode /= 0) return

    ! First the situation where the new dike height is close to the dike toe
    if (dikeHeight <= auxiliaryHeightToe) then
        CoordsAdjusted%N = 2
        call reallocAdjustedCoordinates(CoordsAdjusted, error)
        if (error%errorCode == 0) then
            CoordsAdjusted%y(2) = dikeHeight
            CoordsAdjusted%x(2) = interpolateLine(coordinates%y(1), coordinates%y(2), coordinates%x(1), coordinates%x(2), dikeHeight, error)
            if (error%errorCode /= 0) return
            CoordsAdjusted%x(1) = CoordsAdjusted%x(2) - xDiff_min
            CoordsAdjusted%y(1) = interpolateLine(coordinates%x(1), coordinates%x(2), coordinates%y(1), coordinates%y(2), CoordsAdjusted%x(1), error)
            if (error%errorCode /= 0) return
        else
            return
        endif
    else
        CoordsAdjusted%n = Coordinates%N
        previousWasBerm = .false.
        do i = 2, Coordinates%N - 1
            slope = (coordinates%y(i+1) - coordinates%y(i  )) / (coordinates%x(i+1) - coordinates%x(i  ))
            if (slope < slope_min .and. i < Coordinates%N - 1) then
                !
                ! the next segment of the cross section is a berm segment
                auxiliaryHeightBerm = interpolateLine(coordinates%x(i+1), coordinates%x(i+2), coordinates%y(i+1), coordinates%y(i+2), coordinates%x(i+1) + xDiff_min, error)
                if (error%errorCode /= 0) return
                if (dikeHeight < auxiliaryHeightBerm) then
                    CoordsAdjusted%N = merge(i-1, i, previousWasBerm)
                    exit
                endif
            else
                auxiliaryHeight = interpolateLine(coordinates%x(i-1), coordinates%x(i), coordinates%y(i-1), coordinates%y(i), coordinates%x(i-1) + xDiff_min, error)
                if (error%errorCode /= 0) return
                if (dikeHeight < auxiliaryHeight) then
                    CoordsAdjusted%N = i - 1
                    exit
                elseif (equalRealsRelative(dikeHeight, coordinates%y(i), 1d-12)) then
                    CoordsAdjusted%N = i
                    exit
                elseif (dikeHeight < coordinates%y(i)) then
                    CoordsAdjusted%N = i
                    exit
                endif
            endif
            previousWasBerm = slope < slope_min
        enddo

        !
        ! allocate xCoordsAdjusted and zCoordsAdjusted and check result
        !
        call reallocAdjustedCoordinates(CoordsAdjusted, error)
        if (error%errorCode == 0) then

            ! all segments of the profile except the last
            call copyCoordinates(coordinates, CoordsAdjusted)

            ! last segment of the profile
            CoordsAdjusted%y(CoordsAdjusted%N) = dikeHeight
            CoordsAdjusted%x(CoordsAdjusted%N) = interpolateLine(coordinates%y(CoordsAdjusted%N-1), coordinates%y(CoordsAdjusted%N), &
                coordinates%x(CoordsAdjusted%N-1), coordinates%x(CoordsAdjusted%N), dikeHeight, error)
            if (error%errorCode /= 0) return

            if (CoordsAdjusted%x(CoordsAdjusted%N) < CoordsAdjusted%x(CoordsAdjusted%N-1)) then
                call GetMSGadjusted_xcoordinates(error%Message)
                return
            endif
        else
            return
        endif
    endif
    error%errorCode = 0
end subroutine profileInStructure

subroutine reallocAdjustedCoordinates(CoordsAdjusted, error)
    type(tpCoordinatePair),  intent(inout)  :: coordsAdjusted  !< coordinates in the adjusted profile
    type(tMessage),          intent(inout) :: error

    error%errorCode = 0
    if (allocated(CoordsAdjusted%x)) then
        if (size(CoordsAdjusted%x) == CoordsAdjusted%N) then
            return
        endif
        deallocate(CoordsAdjusted%x, CoordsAdjusted%y)
    end if

    call allocCoordinatePair(CoordsAdjusted, CoordsAdjusted%N, error%errorCode)
    if (error%errorCode /= 0) then
        write(error%Message, GetFMTallocateError()) 2*CoordsAdjusted%N
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
