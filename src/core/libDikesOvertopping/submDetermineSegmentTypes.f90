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

submodule (geometryModuleOvertopping) submDetermineSegmentTypes
   use OvertoppingMessages
   use parametersOvertopping
contains

!> determineSegmentTypes:
!! determine the segment types
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure determineSegmentTypes
!***********************************************************************************************************
   implicit none

   integer  :: i              !< counter dike segments
   logical  :: slopeSegment   !< flag for slope segment
   logical  :: bermSegment    !< flag for berm segment
   integer  :: nBerms         !< total number of berms found; must be <= 2
   character(len=100) :: cfmt !< format string for error message

! ==========================================================================================================

   error%errorCode = 0
   nBerms = 0

   ! loop over dike segments
   do i=1, geometry%Coordinates%N - 1

      ! determine type of dike segment
      slopeSegment = ((geometry%segmentSlopes(i) >= slope_min - marginGrad) .and. &
                      (geometry%segmentSlopes(i) <= slope_max + marginGrad))
      bermSegment  = ((geometry%segmentSlopes(i) >= berm_min  - marginGrad) .and. &
                      (geometry%segmentSlopes(i) <= berm_max  + marginGrad))

      ! add type to structure with geometry data
      if (slopeSegment) then
         geometry%segmentTypes(i) = 1 ! slope
      elseif (bermSegment) then
         geometry%segmentTypes(i) = 2 ! berm
         nBerms = nBerms + 1
         if (i == 1) then
             error%errorCode = 1
             call GetMSG_first_segment_berm(error%Message)
         else if (i == geometry%Coordinates%N - 1) then
             error%errorCode = 1
             call GetMSG_last_segment_berm(error%Message)
         endif
      else
         geometry%segmentTypes(i) = 3 ! other
      endif

   enddo

   if (nBerms > 2) then
       error%errorCode = 1
       call GetFormatTooManyBerms(cfmt)
       write(error%Message, cfmt) nBerms
   endif

end procedure determineSegmentTypes

end submodule submDetermineSegmentTypes
