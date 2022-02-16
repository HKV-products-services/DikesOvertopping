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

submodule (geometryModuleOvertopping) submRemoveSegments
   use OvertoppingMessages
contains

!> removeDikeSegments:
!! remove dike segments
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure removeDikeSegments
!***********************************************************************************************************
   implicit none

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check index starting point new cross section
   if ((index < 1) .or. (index > geometry%Coordinates%N-1)) then
      error%errorCode = 1
      call GetMSGremove_dike_segments_index(error%Message)
   endif

   if (error%errorCode == 0) then

      ! copy the dike normal
      geometryAdjusted%psi = geometry%psi

      ! determine the number of coordinates for the new cross section
      geometryAdjusted%Coordinates%N = geometry%Coordinates%N - index + 1

      ! allocate vectors in structure with geometry data
      call allocateVectorsGeometry (geometryAdjusted%Coordinates%N, geometryAdjusted, error)

      if (error%errorCode == 0) then
         ! select the remaining dike segments for the new cross section
         geometryAdjusted%Coordinates%x    = geometry%Coordinates%x   (index:)
         geometryAdjusted%Coordinates%y    = geometry%Coordinates%y   (index:)
         geometryAdjusted%roughnessFactors = geometry%roughnessFactors(index:)

         ! calculate the differences and segment slopes
         call calculateSegmentSlopes (geometryAdjusted, error)
      endif

      ! determine the type of the segments (slope or berm)
      if (error%errorCode == 0) then
         call determineSegmentTypes (geometryAdjusted, error)
      endif

      ! determine the number of berm segments for the new cross section
      if (error%errorCode == 0) then
         geometryAdjusted%NbermSegments = count(geometryAdjusted%segmentTypes == 2)
      endif

   endif

end procedure removeDikeSegments

end submodule submRemoveSegments
