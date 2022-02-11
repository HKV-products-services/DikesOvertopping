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

submodule (mainModuleOvertopping) submInterpResultsSections
   use OvertoppingMessages
contains

!> interpolateResultsSections:
!! interpolate results for split cross sections
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure interpolateResultsSections
!***********************************************************************************************************
   implicit none
!
   integer        :: i              !< counter dike segments
   real(kind=wp)  :: B              !< width of berm segment (m)
   real(kind=wp)  :: Bsum           !< total width of wide berms (m)
   real(kind=wp)  :: interpFactor   !< interpolation factor

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! -------------------------------------------
   ! calculate the total width of the wide berms
   ! -------------------------------------------

   ! initialize total width of wide berms
   Bsum = 0.0d0

   ! loop over possible berm segments
   do i=2, geometry%Coordinates%N - 2

      ! determine if the current dike segment is a berm segment
      if (geometry%segmentTypes(i) == 2) then

         ! determine the width of the berm segment
         B = geometry%CoordDiff%x(i)

         ! add to total width if the berm segment is a wide berm
         if ((B < L0) .and. (B > 0.25d0*L0)) then
            Bsum = Bsum + B
         endif

      endif

   enddo

   ! -----------------------------------
   ! interpolate the overtopping results
   ! -----------------------------------

   ! calculate interpolation factor
   if ((NwideBerms > 0) .and. (L0 > 0.0d0)) then
      interpFactor = (Bsum - NwideBerms * 0.25d0*L0) / (NwideBerms * 0.75d0*L0)
      if (interpFactor < 0.0d0) error%errorCode = 1
      if (interpFactor > 1.0d0) error%errorCode = 1
   else
      error%errorCode = 1
   endif

   ! interpolate results
   if (error%errorCode == 0) then
      overtopping%z2 = overtoppingB%z2 + interpFactor * (overtoppingF%z2 - overtoppingB%z2)
      overtopping%Qo = overtoppingB%Qo + interpFactor * (overtoppingF%Qo - overtoppingB%Qo)
   else
      call GetMSGinterpolation_error_split_cross_sections(error%Message)
   endif

end procedure interpolateResultsSections

end submodule submInterpResultsSections
