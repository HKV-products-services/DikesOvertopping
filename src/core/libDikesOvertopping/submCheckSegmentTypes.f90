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

submodule (geometryModuleOvertopping) submCheckSegmentTypes
   use OvertoppingMessages
contains

! validation routine for segment types
!!   @ingroup LibOvertopping
module procedure checkSegmentTypes

   ! check segment types
   if (count(geometry%segmentTypes == 3) > 0) then
      error%errorCode = 1
      call GetMSGdike_segment_mismatches(error%Message)
   endif

   ! check number of berm segments
   if (error%errorCode == 0) then
      if (geometry%NbermSegments > 2) then
         error%errorCode = 1
         call GetMSGmax2berm_segments(error%Message)
      endif
   endif

   ! check if first and last dike segment is a slope segment
   if (error%errorCode == 0) then
      if ((geometry%segmentTypes(1) == 2) .or. &
          (geometry%segmentTypes(geometry%Coordinates%N-1) == 2)) then
         error%errorCode = 1
         call GetMSGfirst_and_last_must_be_slope(error%Message)
      endif
   endif
end procedure checkSegmentTypes

end submodule submCheckSegmentTypes

