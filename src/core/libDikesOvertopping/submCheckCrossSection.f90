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

submodule (geometryModuleOvertopping) submCheckCrossSection
   use OvertoppingMessages
   use parametersOvertopping
   use OvertoppingMessages
contains
!> checkCrossSection:
!! check cross section
!!   @ingroup LibOvertopping
module procedure checkCrossSection
!
   implicit none
!
!  Local parameters
!
   type (tpGeometry) :: geometry !< structure with geometry data
   real(kind=wp)     :: rFactor  !< offending roughness factor

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check dike normal
   if ((psi < 0.0d0) .or. (psi > 360.0d0)) then
      error%errorCode = 1
      call GetMSGpsi_not_in_range(error%Message)
   endif

   ! check number of coordinates
   if (error%errorCode == 0) then
      if (coordinates%N < 2) then
         error%errorCode = 1
         call GetMSGdimension_cross_section_less_than_2(error%Message)
      endif
   endif

   ! initialize structure with geometry data
   if (error%errorCode == 0) then
      call initializeGeometry (psi, coordinates, roughnessFactors, geometry, error)
   endif

   ! check minimal distance x-coordinates
   if (error%errorCode == 0) then
      if (minval(geometry%CoordDiff%x) < xDiff_min - marginDiff) then
         error%errorCode = 1
         write (error%Message, GetFMTxcoordinates_must_increase()) xDiff_min
      endif
   endif

   ! check minimal distance y-coordinates
   if (error%errorCode == 0) then
      if (minval(geometry%CoordDiff%y) < 0.0d0) then
         error%errorCode = 1
         call GetMSGycoordinates_must_be_nondecreasing(error%Message)
      endif
   endif

   if (error%errorCode == 0) then
       call checkSegmentTypes(geometry, error)
    endif

   ! check roughness factors
   if (error%errorCode == 0) then
      if (any(geometry%roughnessFactors < rFactor_min)) then
         error%errorCode = 1
         rFactor = minval(geometry%roughnessFactors)
         write (error%Message,GetFMTroughnessfactors_out_of_range()) rFactor_min, rFactor_max, rFactor
      else if (any(geometry%roughnessFactors > rFactor_max)) then
         error%errorCode = 1
         rFactor = maxval(geometry%roughnessFactors)
         write (error%Message,GetFMTroughnessfactors_out_of_range()) rFactor_min, rFactor_max, rFactor
      endif
   endif

   call deallocateGeometry(geometry)

   end procedure checkCrossSection

end submodule subMCheckCrossSection

