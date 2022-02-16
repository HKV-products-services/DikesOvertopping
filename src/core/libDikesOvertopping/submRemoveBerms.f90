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

submodule (geometryModuleOvertopping) submRemoveBerms
   use OvertoppingMessages
contains

!> removeBerms:
!! remove berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure removeBerms
!***********************************************************************************************************
   implicit none
!
   integer        :: i     !< counter dike segments
   integer        :: N     !< counter points cross section without berms
   real(kind=wp)  :: Bsum  !< total berm width (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! determine (x,y)-coordinates cross section without berms
   if (geometry%NbermSegments > 0) then

      ! copy dike normal to structure with new geometry data
      geometryNoBerms%psi = geometry%psi

      ! determine number of coordinates cross section without berms
      geometryNoBerms%Coordinates%N = geometry%Coordinates%N - geometry%NbermSegments

      ! allocate vectors in structure with new geometry data
      call allocateVectorsGeometry (geometryNoBerms%Coordinates%N, geometryNoBerms, error)

      if (error%errorCode == 0) then
         ! initialize total berm width
         Bsum = 0.0d0

         ! add first point to cross section without berms
         N = 1
         geometryNoBerms%Coordinates%x(N) = geometry%Coordinates%x(1)
         geometryNoBerms%Coordinates%y(N) = geometry%Coordinates%y(1)

         ! loop over dike segments
         do i=1, geometry%Coordinates%N - 1

            ! determine if the current dike segment is a berm segment
            if (geometry%segmentTypes(i) == 2) then

               ! check if the berm segment is a horizontal berm
               if (geometry%segmentSlopes(i) > 0.0d0) then
                  error%errorCode = 1
                  call GetMSGRemoveNonHorizontalBerm(error%Message)
               endif

               ! add the width of the berm segment to the total sum
               Bsum = Bsum + geometry%CoordDiff%x(i)

            else

               ! if dike segment is not a berm, then add end point and roughness
               N = N+1
               geometryNoBerms%Coordinates%x(N)       = geometry%Coordinates%x(i+1) - Bsum
               geometryNoBerms%Coordinates%y(N)       = geometry%Coordinates%y(i+1)
               geometryNoBerms%roughnessFactors(N-1)  = geometry%roughnessFactors(i)
         
            endif
         enddo
      endif

      ! calculate the differences and segment slopes
      if (error%errorCode == 0) then
         call calculateSegmentSlopes (geometryNoBerms, error)
      endif

      ! determine the type of the segments (slope or berm)
      if (error%errorCode == 0) then
         call determineSegmentTypes (geometryNoBerms, error)
      endif

      ! determine the number of berm segments
      if (error%errorCode == 0) then
         geometryNoBerms%NbermSegments = count(geometryNoBerms%segmentTypes == 2)
      endif

   else

      ! no berms present
      call copyGeometry (geometry, geometryNoBerms, error)

   endif

end procedure removeBerms

end submodule submRemoveBerms
