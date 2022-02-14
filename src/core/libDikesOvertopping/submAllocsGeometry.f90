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

submodule (geometryModuleOvertopping) submAllocGeometry
   use OvertoppingMessages
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

module procedure copyCoordinates
    integer :: i
    integer :: length

    length = min(size(coordIn%x), size(coordOut%x))
    do i = 1, length
        coordOut%x(i) = coordIn%x(i)
        coordOut%y(i) = coordIn%y(i)
    end do
end procedure copyCoordinates

module procedure allocCoordinatePair
                  allocate(xy%x(n), stat=ierr)
   if (ierr == 0) allocate(xy%y(n), stat=ierr)
end procedure allocCoordinatePair

module procedure cleanupCoordinatePair
   if (allocated(xy%x)) deallocate(xy%x)
   if (allocated(xy%y)) deallocate(xy%y)
end procedure cleanupCoordinatePair

!> deallocateGeometry:
!! deallocate the geometry vectors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure deallocateGeometry
!***********************************************************************************************************
    call cleanupCoordinatePair(geometry%Coordinates)
    call cleanupCoordinatePair(geometry%CoordDiff)
    if (allocated(geometry%roughnessFactors)) deallocate(geometry%roughnessFactors)
    if (allocated(geometry%segmentSlopes))    deallocate(geometry%segmentSlopes)
    if (allocated(geometry%segmentTypes))     deallocate(geometry%segmentTypes)

end procedure deallocateGeometry

!> copyGeometry:
!! copy a geometry structure
!!   @ingroup LibOvertopping
!**********************************************************************************************************
module procedure copyGeometry
!***********************************************************************************************************
   implicit none
!
   integer  :: i  !< counter dike segments

! ==========================================================================================================

   ! allocate vectors in structure with geometry data copy
   call allocateVectorsGeometry (geometry%Coordinates%N, geometryCopy, error)

   if (error%errorCode == 0) then
   ! copy dike normal and number of coordinates to structure with geometry data copy
      geometryCopy%psi           = geometry%psi
      geometryCopy%Coordinates%N = geometry%Coordinates%N

      ! copy (x,y)-coordinates to structure with geometry data copy
      call copyCoordinates(geometry%Coordinates, geometryCopy%Coordinates)
      call copyCoordinates(geometry%CoordDiff, geometryCopy%CoordDiff)

      ! copy roughness factors and segment data to structure with geometry data copy
      do i=1, geometry%Coordinates%N-1
         geometryCopy%roughnessFactors(i) = geometry%roughnessFactors(i)
         geometryCopy%segmentSlopes(i)    = geometry%segmentSlopes(i)
         geometryCopy%segmentTypes(i)     = geometry%segmentTypes(i)
      enddo

      if (present(splitId)) then
         geometryCopy%splitId = splitId
      else
         geometryCopy%splitId = ' '
      endif

      ! copy the number of berm segments to structure with geometry data copy
      geometryCopy%NbermSegments = geometry%NbermSegments
   endif

end procedure copyGeometry

end submodule submAllocGeometry
