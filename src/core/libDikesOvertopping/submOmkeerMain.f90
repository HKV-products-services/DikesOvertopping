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

submodule (omkeerVariantModule) submOmkeerMain
   use geometryModuleOvertopping
contains

!>
!! Subroutine with omkeerVariant
!!
!! @ingroup dllDikesOvertopping
module procedure iterateToGivenDischarge
    type (tpCoordinatePair)                    :: coordinates    !< vector with x/y-coordinates
    type (tpGeometry)                          :: geometry       ! structure with geometry data
!
    !
    ! basic test : water level must be > toe, otherwise return water level
    !
    if (geometryF%ycoords(1) > load%h ) then
        error%errorCode = 0
        dikeHeight     = load%h
        overtopping%z2 = 0d0
        overtopping%Qo = 0d0
    else
        coordinates%N = geometryF%npoints
        coordinates%x = geometryF%xcoords
        coordinates%y = geometryF%ycoords
        call initializeGeometry (geometryF%normal, coordinates, geometryF%roughness, geometry, error)

        if (error%errorCode == 0) then
            call OmkeerValidProfile(load, geometry, givenDischarge, dikeHeight, modelFactors, overtopping, error )
        else
            call set_nan(dikeHeight)
        endif

        call deallocateGeometry(geometry )
        if (associated(geometry%parent)) then
            deallocate(geometry%parent)
        end if
    endif
end procedure iterateToGivenDischarge

end submodule submOmkeerMain
