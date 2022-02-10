submodule (geometryModuleOvertopping) submMergeSequentialBerms
contains

!> mergeSequentialBerms:
!! merge sequential berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure mergeSequentialBerms
!***********************************************************************************************************
   implicit none
!
   integer  :: i                 !< counter dike segments
   logical  :: sequentialBerms   !< flag for sequential berms
   integer  :: index             !< index of first berm
   real(kind=wp) :: B1           !< width of first berm
   real(kind=wp) :: B2           !< width of second berm
   real(kind=wp) :: rFactor1     !< roughness factor of first berm
   real(kind=wp) :: rFactor2     !< roughness factor of second berm

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! ---------------------------------------
   ! determine if there are sequential berms
   ! ---------------------------------------

   ! initialize flag for sequential berms
   sequentialBerms = .false.

   ! loop over possible berm segments
   do i=2, geometry%Coordinates%N - 2

      ! determine if the current and the previous dike segment are sequential berms
      sequentialBerms = ((geometry%segmentTypes(i) == 2) .and. ((geometry%segmentTypes(i-1) == 2)))

      ! if these dike segments are indeed sequential berms then index berms and exit loop
      if (sequentialBerms) then
         index    = i-1
         B1       = geometry%CoordDiff%x(index)
         B2       = geometry%CoordDiff%x(index+1)
         rFactor1 = geometry%roughnessFactors(index)
         rFactor2 = geometry%roughnessFactors(index+1)
         exit
      endif

   enddo

   ! ------------------------------------------
   ! determine structure with new geometry data
   ! ------------------------------------------

   if (.not. sequentialBerms) then

      ! no sequential berms present
      call copyGeometry (geometry, geometryMergedBerms, error)

   else

      ! sequential berms present
      geometryMergedBerms%psi            = geometry%psi
      geometryMergedBerms%Coordinates%N  = geometry%Coordinates%N  - 1
      geometryMergedBerms%NbermSegments  = geometry%NbermSegments - 1

      ! allocate vectors in structure with new geometry data
      call allocateVectorsGeometry (geometryMergedBerms%Coordinates%N, geometryMergedBerms, error)

      if (error%errorCode == 0) then
         ! remove x-coordinate between sequential berms
         geometryMergedBerms%Coordinates%x(1:index)  = geometry%Coordinates%x(1:index)
         geometryMergedBerms%Coordinates%x(index+1:) = geometry%Coordinates%x(index+2:)

         ! remove y-coordinate between sequential berms
         geometryMergedBerms%Coordinates%y(1:index)  = geometry%Coordinates%y(1:index)
         geometryMergedBerms%Coordinates%y(index+1:) = geometry%Coordinates%y(index+2:)

         ! calculate the influence factor for the compound berm
         geometryMergedBerms%roughnessFactors(1:index-1) = geometry%roughnessFactors(1:index-1)
         if ((B1+B2) > 0.0d0) then
            geometryMergedBerms%roughnessFactors(index)  = (B1*rFactor1 + B2*rFactor2) / (B1+B2)
         else
            error%errorCode = 1
            call GetMSGmerging_seq_berm(error%Message)
         endif
         geometryMergedBerms%roughnessFactors(index+1:)  = geometry%roughnessFactors(index+2:)
      endif
      
      ! recalculate differences and slopes
      if (error%errorCode == 0) then
         call calculateSegmentSlopes (geometryMergedBerms, error)
      endif

      ! remove sequential berm from dike segment types
      geometryMergedBerms%segmentTypes(1:index)  = geometry%segmentTypes(1:index)
      geometryMergedBerms%segmentTypes(index+1:) = geometry%segmentTypes(index+2:)

   endif

end procedure mergeSequentialBerms

end submodule submMergeSequentialBerms
