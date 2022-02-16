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

submodule (geometryModuleOvertopping) submCopyGeom
   !use OvertoppingMessages
   !use OvertoppingMessages
contains

!> copyGeometry:
!! copy a geometry structure
!!   @ingroup LibOvertopping
!**********************************************************************************************************
module procedure copyGeometry
!***********************************************************************************************************
   implicit none
!
   integer  :: i  !< counter dike segments

! ==========================================================================================================

   ! allocate vectors in structure with geometry data copy
   call allocateVectorsGeometry (geometry%Coordinates%N, geometryCopy, error)

   if (error%errorCode == 0) then
   ! copy dike normal and number of coordinates to structure with geometry data copy
      geometryCopy%psi           = geometry%psi
      geometryCopy%Coordinates%N = geometry%Coordinates%N

      ! copy (x,y)-coordinates to structure with geometry data copy
      call copyCoordinates(geometry%Coordinates, geometryCopy%Coordinates)
      call copyCoordinates(geometry%CoordDiff, geometryCopy%CoordDiff)

      ! copy roughness factors and segment data to structure with geometry data copy
      do i=1, geometry%Coordinates%N-1
         geometryCopy%roughnessFactors(i) = geometry%roughnessFactors(i)
         geometryCopy%segmentSlopes(i)    = geometry%segmentSlopes(i)
         geometryCopy%segmentTypes(i)     = geometry%segmentTypes(i)
      enddo

      if (present(splitId)) then
         geometryCopy%splitId = splitId
      else
         geometryCopy%splitId = ' '
      endif

      ! copy the number of berm segments to structure with geometry data copy
      geometryCopy%NbermSegments = geometry%NbermSegments
   endif

end procedure copyGeometry

end submodule submCopyGeom
