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

submodule (geometryModuleOvertopping) submAllocVectorsGeom
   use OvertoppingMessages
contains

!> allocateVectorsGeometry:
!! allocate the geometry vectors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure allocateVectorsGeometry
!***********************************************************************************************************
   implicit none
!
!  local parameters
!
   integer  :: ierr(5)        !< error code allocate
   integer  :: sizeArrays     !< total size of arrays to be allocated

! ==========================================================================================================

   if (allocated(geometry%Coordinates%x)) then
       if (size(geometry%Coordinates%x) == nCoordinates) then
           return
       else
           call deallocateGeometry(geometry)
       end if
   end if

   call allocCoordinatePair (geometry%Coordinates, nCoordinates, ierr(1))
   call allocCoordinatePair (geometry%CoordDiff, nCoordinates-1, ierr(2))
   allocate (geometry%roughnessFactors (nCoordinates-1), stat=ierr(3))
   allocate (geometry%segmentSlopes    (nCoordinates-1), stat=ierr(4))
   allocate (geometry%segmentTypes     (nCoordinates-1), stat=ierr(5))

   error%errorCode = merge(0, 1, all(ierr==0))
   if (error%errorCode /= 0) then
       sizeArrays  = 2 * nCoordinates + 5 * (nCoordinates-1)
       write(error%Message, GetFMTallocateError()) sizeArrays
   endif

end procedure allocateVectorsGeometry

end submodule submAllocVectorsGeom
