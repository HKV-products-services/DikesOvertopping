submodule (geometryModuleOvertopping) submAllocGeometry
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
   integer  :: ierr           !< error code allocate
   integer  :: sizeArrays     !< total size of arrays to be allocated

! ==========================================================================================================

   if (allocated(geometry%Coordinates%x)) then
       if (size(geometry%Coordinates%x) == nCoordinates) then
           return
       else
           call deallocateGeometry(geometry)
       end if
   end if

                  allocate (geometry%Coordinates%x    (nCoordinates  ), stat=ierr)
   if (ierr == 0) allocate (geometry%Coordinates%y    (nCoordinates  ), stat=ierr)
   if (ierr == 0) allocate (geometry%roughnessFactors (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%CoordDiff%x      (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%CoordDiff%y      (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%segmentSlopes    (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%segmentTypes     (nCoordinates-1), stat=ierr)

   error%errorCode = ierr
   if (ierr /= 0) then
       sizeArrays  = 2 * nCoordinates + 5 * (nCoordinates-1)
       write(error%Message, GetFMTallocateError()) sizeArrays
   endif

end procedure allocateVectorsGeometry

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
      do i=1, geometry%Coordinates%N
         geometryCopy%Coordinates%x(i) = geometry%Coordinates%x(i)
         geometryCopy%Coordinates%y(i) = geometry%Coordinates%y(i)
      enddo

      ! copy roughness factors and segment data to structure with geometry data copy
      do i=1, geometry%Coordinates%N-1
         geometryCopy%roughnessFactors(i) = geometry%roughnessFactors(i)
         geometryCopy%CoordDiff%x(i)      = geometry%CoordDiff%x(i)
         geometryCopy%CoordDiff%y(i)      = geometry%CoordDiff%y(i)
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
