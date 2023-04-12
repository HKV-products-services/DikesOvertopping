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

submodule (geometryModuleOvertopping) submSplitCrossSection
   use OvertoppingMessages
contains

!> splitCrossSection:
!! split a cross section
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure splitCrossSection
!***********************************************************************************************************
   implicit none
!
   integer        :: i           !< counter dike segments
   real(kind=wp)  :: B           !< width of berm segment (m)
   real(kind=wp)  :: horzShift   !< horizontal shift to adapt wide berms (m)
   real(kind=wp)  :: vertShift   !< vertical shift to adapt wide berms (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! initialize the structures with geometry data for split cross sections
   call copyGeometry (geometry, geometrySectionB, error, 'B')
   if (error%errorCode == 0) call copyGeometry (geometry, geometrySectionF, error, 'F')

   ! initialize number of wide berms
   NwideBerms = 0

   ! ------------------------------------------------------------
   ! determine the (x,y)-coordinates for the split cross sections
   ! ------------------------------------------------------------

   if (error%errorCode == 0) then
      ! loop over possible berm segments
      do i=2, geometry%Coordinates%N - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! determine the width of the berm segment
            B = geometry%CoordDiff%x(i)

            ! determine if the berm segment is a wide berm
            if ((B < L0) .and. (B > 0.25d0*L0)) then

               ! count the number of wide berms
               NwideBerms = NwideBerms+1

               ! adapt berm width to B=L0/4 for cross section with ordinary berms

               ! shift start point berm along the segment to reduce width to B=L0/4
               horzShift = (B-0.25d0*L0)
               vertShift = (B-0.25d0*L0) * geometry%segmentSlopes(i)
               geometrySectionB%Coordinates%x(i)     = geometrySectionB%Coordinates%x(i)     + horzShift
               geometrySectionB%Coordinates%y(i)     = geometrySectionB%Coordinates%y(i)     + vertShift

               ! shift preceding points horizontally to keep original segment slopes
               if (geometry%segmentSlopes(i-1) > 0.0d0) then
                  horzShift = (B-0.25d0*L0) * (1 - geometry%segmentSlopes(i)/geometry%segmentSlopes(i-1))
               else
                  horzShift = 0.0d0 
                  error%errorCode = 1 ! previous dike segment has a gradient <= zero
                  call GetMSGsplit_cross_section_seq_berm(error%Message)
               endif
               if (horzShift < 0.0d0) then
                  error%errorCode = 1 ! previous dike segment has a more gentle slope
                  call GetMSGsplit_cross_section_seq_berm(error%Message)
               endif
               geometrySectionB%Coordinates%x(1:i-1) = geometrySectionB%Coordinates%x(1:i-1) + horzShift

               ! adapt berm width to B=L0 for cross section with foreshores
               horzShift = B-L0
               geometrySectionF%Coordinates%x(1:i)   = geometrySectionF%Coordinates%x(1:i)   + horzShift

            endif

         endif

      enddo
   
   endif

   ! ---------------------------------------------------------------
   ! recalculate differences and slopes for the split cross sections
   ! ---------------------------------------------------------------

   ! calculate the differences and segment slopes
   if (NwideBerms > 0) then
      if (error%errorCode == 0) call calculateSegmentSlopes (geometrySectionB, error)
      if (error%errorCode == 0) call calculateSegmentSlopes (geometrySectionF, error)
   end if

   ! type of segments and number of berm segments is NOT recalculated

end procedure splitCrossSection

end submodule submSplitCrossSection
