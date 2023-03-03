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

submodule (geometryModuleOvertopping) submInitializeGeometry
   use parametersOvertopping
   use OvertoppingMessages
contains

!> initializeGeometry:
!! initialize the geometry
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure initializeGeometry
!***********************************************************************************************************
   implicit none
!
!  local parameters
!
   integer                          :: i                                !< loop counter
   integer                          :: ierr                             !< error code

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! copy dike normal to structure with geometry data
   geometry%psi = psi

   ! copy number of coordinates to structure with geometry data
   geometry%Coordinates%N = coordinates%N

   ! allocate vectors in structure with geometry data
   call allocateVectorsGeometry (coordinates%N, geometry, error)

   if (error%errorCode == 0) then
   ! copy (x,y)-coordinates and roughness factors to structure with geometry data
      geometry%Coordinates%x     = Coordinates%x
      geometry%Coordinates%y     = Coordinates%y
      geometry%roughnessFactors = roughnessFactors

   ! calculate the differences and segment slopes
      call calculateSegmentSlopes (geometry, error)
   endif

   ! determine the type of the segments (slope or berm)
   if (error%errorCode == 0) then
      call determineSegmentTypes (geometry, error)
   endif

   if (error%errorCode == 0) then
       do i = 1, geometry%Coordinates%N - 1
           if (geometry%Roughnessfactors(i) < rFactor_min .or. geometry%Roughnessfactors(i) > rFactor_max) then
               error%errorCode = 1
               write(error%Message, GetFMTroughnessfactors_out_of_range(), iostat=ierr) &
                   rFactor_min, rFactor_max, geometry%Roughnessfactors(i)
               if (ierr /= 0) then
                   error%Message = GetFMTroughnessfactors_out_of_range()
               endif
               exit
           endif
       enddo
   endif 

   ! determine the number of berm segments
   if (error%errorCode == 0) then
      geometry%NbermSegments = count(geometry%segmentTypes == 2)
   endif
end procedure initializeGeometry

end submodule submInitializeGeometry
