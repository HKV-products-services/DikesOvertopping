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

submodule (geometryModuleOvertopping) submAdjNonHorizBerms
   use OvertoppingMessages
contains

!> adjustNonHorizontalBerms:
!! adjust non-horizontal berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure adjustNonHorizontalBerms
!***********************************************************************************************************
   implicit none
!
   integer        :: i     !< counter dike segments
   real(kind=wp)  :: hBerm !< average berm height (m)
   real(kind=wp)  :: dx1   !< horizontal distance from previous point to starting point horizontal berm (m)
   real(kind=wp)  :: dx2   !< horizontal distance from end point horizontal berm to next point (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! initialize the structure with geometry data with horizontal berms
   call copyGeometry (geometry, geometryFlatBerms, error, geometry%splitId)

   if (error%errorCode == 0) then
      ! loop over possible berm segments
      do i=2, geometry%Coordinates%N - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! calculate average berm height
            hBerm = (geometry%Coordinates%y(i)+geometry%Coordinates%y(i+1))/2

            ! calculate horizontal distance from previous point to starting point horizontal berm
            if (geometryFlatBerms%segmentSlopes(i-1) > 0.0d0) then
               dx1 = (hBerm - geometryFlatBerms%Coordinates%y(i-1)) / geometryFlatBerms%segmentSlopes(i-1)
            else
               error%errorCode = 1
            endif

            ! calculate horizontal distance from end point horizontal berm to next point
            if (geometryFlatBerms%segmentSlopes(i+1) > 0.0d0) then
               dx2 = (geometryFlatBerms%Coordinates%y(i+1) - hBerm) / geometryFlatBerms%segmentSlopes(i+1)
            else
               error%errorCode = 1
            endif

            ! ---------------------------------------------------------------------------------------------
            ! NOTE : for calculation of dx1 and dx2 the slopes of the previous and next dike segment should
            !        have a gradient greather than zero. Therefore, sequential berms should be merged first
            ! ---------------------------------------------------------------------------------------------

            ! determine possible error message
            if (error%errorCode /= 0) then
               call GetMSGadjust_non_horizontal_seq_berm(error%Message)
               exit ! exit loop
            endif

            ! determine new starting point horizontal berm
            geometryFlatBerms%Coordinates%y(i)   = hBerm
            geometryFlatBerms%Coordinates%x(i)   = geometryFlatBerms%Coordinates%x(i-1) + dx1

            ! determine new end point horizontal berm
            geometryFlatBerms%Coordinates%y(i+1) = hBerm
            geometryFlatBerms%Coordinates%x(i+1) = geometryFlatBerms%Coordinates%x(i+1) - dx2

            ! recalculate segment slopes
            call calculateSegmentSlopes (geometryFlatBerms, error)
            if (error%errorCode /= 0) then
               exit ! exit loop
            endif

         endif

      enddo

   endif

end procedure adjustNonHorizontalBerms

end submodule submAdjNonHorizBerms
