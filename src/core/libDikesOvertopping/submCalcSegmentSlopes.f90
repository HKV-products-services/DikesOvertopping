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

submodule (geometryModuleOvertopping) submCalcSegmentSlopes
   use OvertoppingMessages
contains

!> calculateSegmentSlopes:
!! calculate the segment slopes
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateSegmentSlopes
!***********************************************************************************************************
   implicit none

   ! initialize flag for succes and error message
   error%errorCode = 0
   if (geometry%Coordinates%N < 2) then
       error%errorCode = 1
       call GetMSGdimension_cross_section_less_than_2(error%Message)
   else

      ! calculate horizontal distances
      geometry%CoordDiff%x = geometry%Coordinates%x(2:geometry%Coordinates%N  ) - &
                             geometry%Coordinates%x(1:geometry%Coordinates%N-1) 

      ! calculate vertical distances
      geometry%CoordDiff%y = geometry%Coordinates%y(2:geometry%Coordinates%N  ) - &
                             geometry%Coordinates%y(1:geometry%Coordinates%N-1) 

      ! calculate the segment slopes
      if (minval(geometry%CoordDiff%x) > 0.0d0) then
         geometry%segmentSlopes = geometry%CoordDiff%y / geometry%CoordDiff%x
      else
         error%errorCode = 1
         call GetMSGslope_negative(error%Message)
      endif
   endif

end procedure calculateSegmentSlopes

end submodule submCalcSegmentSlopes

